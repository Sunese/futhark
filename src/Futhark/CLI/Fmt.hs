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
import System.Exit
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

isLocatedBefore :: L Token -> UncheckedDec -> Bool
isLocatedBefore comment dec = locOf dec > unpackTokLoc comment

-- check if there has been a comment before the dec 
-- if so, just insert above the dec
-- if there has not been a comment before, just do ppr dec
flush :: [UncheckedDec] -> [L Token] -> [Doc] -> [Doc]
flush decs comments doc = do
  case decs of
    [] -> do -- no more decs, check if there are comments
      case comments of
        [] -> doc -- no decs and no comments, return constructed doc
        _ -> do -- no decs, but some comments, append comment string and recurse
          flush 
              decs
              (tail comments)
              -- tmp doc ++ comment (reversed)
              (doc ++ [text (unpackCommentString (head comments))])
    _ -> do -- some decs, check if there are comments
      case comments of
        [] -> do -- no more comments, but still some decs, ppr curr dec and recurse
          flush (tail decs) comments (doc ++ [ppr $ head decs])
        _ -> do -- there are both decs and comments, check if commentbefore curr dec
          if head comments `isLocatedBefore` head decs then do 
            flush 
              (tail decs) 
              (tail comments)
              -- tmp doc ++ comment ++ curr dec
              (doc
              ++ [string (unpackCommentString (head comments))]
              ++ [ppr $ head decs])
          else do -- no comment before current dec, ppr curr dec and recurse
           flush 
            (tail decs) 
            comments 
            (doc ++ [ppr $ head decs])

ppr' :: [UncheckedDec] -> [L Token] -> Doc
ppr' decs comments = stack $ punctuate line $ flush decs comments []

-- | Run @futhark fmt@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      s <- TIO.readFile file
      case parseWithComments file s of
        (Left e, _) -> do
          putStr $ "Syntax error: " ++ locStr (syntaxErrorLoc e) ++ "\n" ++ syntaxErrorMsg e 
          putStrLn "File has not been changed."
        (Right prog, comments) -> do
        --(Right prog, comments) -> do
          --TIO.putStrLn $ prettyText prog
          case prog of
            Prog doc decs -> do
              putStrLn $ pretty $ ppr doc <> ppr' decs comments
              --putStrLn $ pretty $ head $ flush decs comments []

              --TIO.putStrLn $ format (T.lines $ T.pack prog') [] decs comments
          -- instance is in language/futhark/pretty.hs line no. 427
          --print prog


          -- 1st method: change up the ppr function/ or Pretty instances such that the instance that runs through CST also prints the comments
          -- 2nd method: leave ppr/Pretty instances as is, and insert comments into ppr'd program
          -- 1st method is preferred - fmt.go does it this way, and it's also more efficient

    _ -> Nothing
  