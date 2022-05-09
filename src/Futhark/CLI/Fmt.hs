{-# LANGUAGE OverloadedStrings #-}

-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import Data.Text (Text, lines, unlines)
import qualified Data.Text.IO as T
import Futhark.Util.Loc
import Futhark.Util.Options ( mainWithOptions )
import Language.Futhark
import Language.Futhark.Parser (parseWithComments, SyntaxError (syntaxErrorMsg, syntaxErrorLoc))
import Language.Futhark.Parser.Lexer.Tokens
import Prelude hiding (writeFile, unlines, lines)
import System.Exit

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

unpackDecEndLine :: UncheckedDec -> Int
unpackDecEndLine dec = do
  case locOf dec of
    NoLoc -> error "NoLoc"
    Loc _ end -> 
      case end of
        Pos _ line _ _ -> line

format :: [Text] -> [Text] -> [UncheckedDec] -> [L Token] -> Text
format srcLines srcLinesRest decs comments = do
  case decs of
    [] -> do
      case comments of
        [] -> unlines $ srcLines ++ srcLinesRest
        _ -> do -- no more decs, but still some comments, just consume the rest
          unlines $ srcLines ++ srcLinesRest
    _ -> do
      case comments of
        [] -> unlines $ srcLines ++ srcLinesRest -- no more comments, but still some decs, just consume the rest
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

-- | Run @futhark fmt@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      s <- T.readFile file
      case parseWithComments file s of
          (Left e, _) -> do
            putStr $ "Syntax error: " ++ locStr (syntaxErrorLoc e) ++ "\n" ++ syntaxErrorMsg e 
            putStrLn "File has not been changed."
            exitWith $ ExitFailure 2
          (Right Prog { progDoc = _, progDecs = decs }, comments) -> do
            --Data.Text.IO.writeFile file $ format [] (lines s) decs comments
            T.putStrLn $ format [] (lines s) decs comments
    _ -> Nothing