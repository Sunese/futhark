{-# LANGUAGE OverloadedStrings #-}

-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import Data.Text (Text, append, pack, take, takeEnd, strip, unpack)
import qualified Data.Text.IO as T
import Data.Function (fix)
import Futhark.Util.Loc
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyText)
import Language.Futhark
import Language.Futhark.Parser (parseWithComments, SyntaxError (syntaxErrorMsg, syntaxErrorLoc))
import Language.Futhark.Parser.Lexer.Tokens
import Prelude hiding (take, writeFile)
import System.Exit
import System.IO
import Data.Text.IO (writeFile)


import Data.List ( map, length, head, tail )
import Data.Aeson.Key (toString)
import Control.Monad.Cont (MonadIO(liftIO))

unpackCommentString :: L Token -> String
unpackCommentString (L _ (COMMENT s)) = s
unpackCommentString _ = error "unpackCommentString: not a comment"

unpackTokenLoc :: L Token -> Loc
unpackTokenLoc (L loc a) = loc

unpackColTok :: L Token -> Int
unpackColTok tok = do
  case tok of 
    L loc _ -> 
      case loc of
        NoLoc -> -1
        Loc _ end -> 
          case end of
            Pos _ _ col _ -> col

unpackColDec :: UncheckedDec -> Int
unpackColDec dec = do
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
                      Pos _ _ col _ -> col
    _ -> -1

-- check if there has been a comment before the given token
hasCommentBefore :: UncheckedDec -> L Token -> Bool
hasCommentBefore dec comment =
  locOf dec > unpackTokenLoc comment

noOfCharacters :: [UncheckedDec] -> Int
noOfCharacters queue = do
  sum (map unpackColDec queue) + length queue

format ::
  Text ->
  [UncheckedDec] ->
  [UncheckedDec] ->
  [UncheckedDec] ->
  Text ->
  Text ->
  [L Token] ->
  Text
format s decs decs' decsQueue programText rest comments = do
  case decs' of
      [] -> programText
      _ -> do
        case comments of
          [] -> append programText rest
          _ -> do
            if hasCommentBefore (head decs') (head comments)
              then do
                format
                  s
                  decs
                  (tail decs')
                  (head decs' : decsQueue)
                  (append
                    (append programText (take (noOfCharacters decsQueue) s))
                    (append (pack $ unpackCommentString (head comments)) "\n"))
                  (takeEnd (noOfCharacters decs - noOfCharacters decsQueue) s)
                  (tail comments)
            else do
              format 
                s
                decs
                (tail decs')
                (head decs' : decsQueue)
                programText 
                rest
                comments

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
            --Data.Text.IO.writeFile file $ format s decs decs [] "" s comments
            T.putStr $ format s decs decs [] "" s comments
    _ -> Nothing