{-# LANGUAGE OverloadedStrings #-}

-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where


import Language.Futhark.Parser (parseWithComments)
import Data.Function (fix)
import qualified Data.Text.IO as T
import Futhark.Util.Loc
import Futhark.Util.Options
import Futhark.Util.Pretty (prettyText)
import Language.Futhark
import Language.Futhark.Parser.Lexer.Tokens
import System.Exit
import System.IO
import Data.Text
import Data.List
import Data.Loc

unpackCommentString :: L Token -> String
unpackCommentString (L _ (COMMENT s)) = s
unpackCommentString _ = error "unpackCommentString: not a comment"

unpackTokenLoc :: L Token -> Loc
unpackTokenLoc (L loc a) = loc

unpackColumn :: L Token -> Int
unpackColumn tok = do
  case tok of 
    L loc _ -> 
      case loc of
        NoLoc -> -1
        Loc _ end -> 
          case end of
            Pos _ _ col _ -> col

unpackDec :: UncheckedDec -> Int
unpackDec dec = do
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

calculateTakeAmount :: [UncheckedDec] -> Int
calculateTakeAmount queue = do
  sum (Data.List.map unpackDec queue) + Data.List.length queue - 1

flush :: 
  Text -> 
  [UncheckedDec] -> 
  [UncheckedDec] -> 
  Text -> 
  [L Token] -> 
  IO ()
flush s decs decsQueue programText comments = do
  -- check all decs
  -- if one of them hasCommentBefore, then 'take' until that comment and
  -- concatenate comment literal/string onto taken stuff and
  -- proceed onto checking next dec

  -- calculate how many chars to take by checking end position of each declaration (col) + 1 for linebreak
  case decs of
      [] -> T.putStrLn "decs empty"
      _ -> do
        case comments of
          [] -> T.putStrLn "comments empty"
          _ -> do
            if hasCommentBefore (Data.List.head decs) (Data.List.head comments) 
              then do
                T.putStrLn $ Data.Text.take (calculateTakeAmount decsQueue) s
                -- something like programText ++ (Data.Text.take (calculateTakeAmount decsQueue) s)
            else do
              flush s (Data.List.tail decs) (Data.List.head decs : decsQueue) programText comments
    

-- | Run @futhark fmt@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      s <- T.readFile file
      case parseWithComments file s of
          (Left e, _) -> do
            hPutStrLn stderr "Parse error: "
            exitWith $ ExitFailure 2
          (Right Prog { progDoc = doc, progDecs = decs }, comments) -> do
            flush s decs [] "" comments
    _ -> Nothing