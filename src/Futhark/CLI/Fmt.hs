{-# LANGUAGE OverloadedStrings #-}

-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import Data.Text (Text, append, pack, take, takeEnd, strip, unpack, stripEnd, unlines, lines)
import qualified Data.Text.IO as T
import Data.Function (fix)
import Futhark.Util.Loc
import Futhark.Util.Options ( mainWithOptions )
import Futhark.Util.Pretty (prettyText)
import Language.Futhark
import Language.Futhark.Parser (parseWithComments, SyntaxError (syntaxErrorMsg, syntaxErrorLoc))
import Language.Futhark.Parser.Lexer.Tokens
import Prelude hiding (writeFile, unlines, lines)
import System.Exit
import System.IO
import Data.Text.IO (writeFile)


import Data.List ( map, length, head, tail )
import Data.Aeson.Key (toString)
import Control.Monad.Cont (MonadIO(liftIO))
--import qualified System.Posix.Internals as T

unpackTokenLoc :: L Token -> Loc
unpackTokenLoc (L loc _) = loc

unpackTokLine :: L Token -> Int
unpackTokLine tok = do
  case tok of 
    L loc _ -> 
      case loc of
        NoLoc -> -1
        Loc _ end -> 
          case end of
            Pos _ line _ _ -> line

unpackDecEndLine :: UncheckedDec -> Int
unpackDecEndLine dec = do
  case dec of
    ValDec vbb -> case vbb of
      ValBind _ _ _ _ _ _ _ _ _ sl -> 
        case sl of
          sl' -> 
            case sl' of
              SrcLoc loc -> 
                case loc of
                  NoLoc -> -1
                  Loc _ end -> 
                    case end of
                      Pos _ line _ _ -> line
    _ -> -1

format :: [Text] -> [Text] -> [UncheckedDec] -> [L Token] -> Text
format lines' linesRest decs comments = do
  case decs of
    [] -> do
      case comments of
        [] -> unlines $ lines' ++ linesRest
        _ -> do -- no more decs, but still some comments, just eat the rest
          unlines $ lines' ++ linesRest
    _ -> do
      case comments of
        [] -> unlines $ lines' ++ linesRest -- no more comments, but still some decs, just eat the rest
        _ -> do
          if locOf (head decs) > unpackTokenLoc (head comments) then do
            let commentLine = unpackTokLine (head comments)
            let (a, linesRest') = Prelude.splitAt (commentLine - length lines') linesRest
            let lines'' = lines' ++ a
            format lines'' linesRest' (tail decs) (tail comments)
          else do
            -- get end line pos of dec
            let decLineNo = unpackDecEndLine (head decs)
            let (lines'', linesRest') = Prelude.splitAt (decLineNo - length lines') linesRest
            let lines''' = lines' ++ lines''
            format lines''' linesRest' (tail decs) comments

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
          (Right Prog { progDoc = doc, progDecs = decs }, comments) -> do
            --Data.Text.IO.writeFile file $ format [] (lines s) decs comments
            T.putStrLn $ format [] (lines s) decs comments
    _ -> Nothing