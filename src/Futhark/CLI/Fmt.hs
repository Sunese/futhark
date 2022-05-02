{-# LANGUAGE OverloadedStrings #-}

-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import Language.Futhark.Parser (parseWithComments)
import Data.Function (fix)
import qualified Data.Text.IO as T
import Futhark.Util.Loc
import Futhark.Util.Options
import Language.Futhark
import Language.Futhark.Parser.Lexer.Tokens

unpackCommentString :: L Token -> String
unpackCommentString (L _ (COMMENT s)) = s
unpackCommentString _ = error "unpackCommentString: not a comment"

unpackTokenLoc :: L Token -> Loc
unpackTokenLoc (L loc _) = loc

-- check if there has been a comment before the given token
hasCommentBefore :: UncheckedDec -> L Token -> Bool
hasCommentBefore dec comment =
  locOf dec > unpackTokenLoc comment

flush decs comments = do
  fix $ \loop -> do
    let dec = head decs
    let comment = head comments
    if hasCommentBefore dec comment then do 
      -- insert comment into IO or something
      (decs', comments') <- loop
      (tail decs', tail comments')
    else do 
      (decs', comments') <- loop
      (tail decs', comments')

-- | Run @futhark fmt@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      s <- T.readFile file
      case parseWithComments file s of
          (Left _, _) -> do
            T.putStrLn "error"
          (Right (Prog _ decs), comments) -> do
            T.putStrLn "error"


          
    _ -> Nothing