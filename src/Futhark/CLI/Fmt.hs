{-# LANGUAGE OverloadedStrings #-}

-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import Language.Futhark.Parser (parseWithComments)
import qualified Data.Text.IO as T
import Futhark.Util.Loc
import Futhark.Util.Options
import Language.Futhark
import Language.Futhark.Parser.Lexer.Tokens

unpackCommentString :: L Token -> String
unpackCommentString (L _ (COMMENT s)) = s
unpackCommentString _ = error "unpackCommentString: not a comment"

unpackCommentLoc :: L Token -> Loc
unpackCommentLoc (L loc _) = loc

-- | Run @futhark fmt@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      s <- T.readFile file
      case parseWithComments file s of
        (Left _, _) -> do
          T.putStrLn "error"
        (Right Prog { progDoc = doc, progDecs = decs }, comments) -> do
          case comments of
            [] -> T.putStrLn "no comments"
            _ -> mapM_ (putStrLn . unpackCommentString) comments
    _ -> Nothing