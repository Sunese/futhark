{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import Data.Loc (Loc(..))
import qualified Data.Text as T hiding (head, tail)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LTIO
import Futhark.Util.Loc hiding (SrcLoc)
import Futhark.Util.Options ( mainWithOptions )
import Language.Futhark hiding (pretty)
import Language.Futhark.Parser (parseWithComments, SyntaxError (syntaxErrorMsg, syntaxErrorLoc))
import Language.Futhark.Parser.Lexer.Tokens
import Prelude hiding (writeFile, unlines, lines)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Exit
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Complex (Complex, realPart, imagPart)
import Data.Ratio (Ratio(..), denominator, numerator)
import Futhark.Util.Pretty

unpackTokLoc :: L Token -> Loc
unpackTokLoc (L loc _) = loc

unpackTokEndLine :: L Token -> Int
unpackTokEndLine tok = do
  case tok of 
    L loc _ -> 
      case loc of
        NoLoc -> error "NoLoc"
        Loc _ end -> 
          case end of
            Pos _ line _ _ -> line

unpackCommentString (L _ (COMMENT s)) = s
unpackCommentString _ = error "unpackCommentString: not a comment"

unpackDecEndLine :: UncheckedDec -> Int
unpackDecEndLine dec = do
  case locOf dec of
    NoLoc -> error "NoLoc"
    Loc _ end -> 
      case end of
        Pos _ line _ _ -> line

format :: [T.Text] -> [T.Text] -> [UncheckedDec] -> [L Token] -> T.Text
format srcLines srcLinesRest decs comments = do
  case decs of
    [] -> do
      case comments of
        [] -> T.unlines $ srcLines ++ srcLinesRest
        _ -> do -- no more decs, but still some comments, just consume the rest
          let srcLinesRest' = srcLines ++ [T.pack $ unpackCommentString $ head comments]
          T.unlines $ srcLines ++ srcLinesRest
    _ -> do
      case comments of
        [] -> T.unlines $ srcLines ++ srcLinesRest -- no more comments, but still some decs, just consume the rest
        _ -> do
          if locOf (head decs) > unpackTokLoc (head comments) then do
            let commentEndLine = unpackTokEndLine (head comments)
            let (srcLines', srcLinesRest') = Prelude.splitAt (commentEndLine - length srcLines) srcLinesRest
            let srcLines'' = srcLines ++ srcLines'
            format srcLines'' srcLinesRest' (tail decs) (tail comments)
          else do
            let decLineNo = unpackDecEndLine (head decs)
            let (srcLines', srcLinesRest') = Prelude.splitAt (decLineNo - length srcLines) srcLinesRest
            let srcLines'' = srcLines ++ srcLines'
            format srcLines'' srcLinesRest' (tail decs) comments

hasCommentBefore :: UncheckedDec -> L Token -> Bool
hasCommentBefore dec com = locOf dec > unpackTokLoc com

-- check if there has been a comment before the dec 
-- if so, just insert above the dec
-- if there has not been a comment before, just do ppr dec
flush :: [UncheckedDec] -> [L Token] -> [Doc] -> [Doc]
flush decs comments doc = do
  case decs of
    [] -> doc -- TODO
    _ -> do
      case comments of
        [] -> doc -- TODO
        _ -> do
          if hasCommentBefore (head decs) (head comments) then do 
            flush (tail decs) (tail comments) (doc ++ [ppr (head decs)]) --TODO
          else do 
           doc -- TODO
  



ppr' :: [UncheckedDec] -> [L Token] -> Doc
ppr' decs comments = stack . punctuate line . flush decs comments []
   

-- | Run @futhark fmt@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      s <- TIO.readFile file
      case parseWithComments file s of
        (Left e, _) -> do
          exitWith $ ExitFailure 2
        (Right prog, comments) -> do
        --(Right prog, comments) -> do
          --TIO.putStrLn $ prettyText prog
          case prog of 
            Prog doc decs -> do
              putStrLn $ pretty $ ppr doc <> ppr' decs
              --TIO.putStrLn $ format (T.lines $ T.pack prog') [] decs comments
          -- instance is in language/futhark/pretty.hs line no. 427
          --print prog


          -- 1st method: change up the ppr function/ or Pretty instances such that the instance that runs through CST also prints the comments
          -- 2nd method: leave ppr/Pretty instances as is, and insert comments into ppr'd program
          -- 1st method is preferred - fmt.go does it this way, and it's also more efficient

    _ -> Nothing
  