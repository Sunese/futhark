-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import Data.List (isSuffixOf)
import Data.Text (dropEnd, pack, unpack)
import qualified Data.Text.IO as TIO
import Futhark.Util.Loc hiding (SrcLoc)
import Futhark.Util.Options (mainWithOptions)
import Futhark.Util.Pretty
import Language.Futhark hiding (pretty)
import Language.Futhark.Parser (SyntaxError (syntaxErrorLoc, syntaxErrorMsg), parseWithComments)
import Language.Futhark.Parser.Lexer.Tokens
import System.IO (writeFile)
import qualified Text.PrettyPrint.Mainland as PP
import Prelude hiding (lines, unlines, writeFile)


unpackTokLoc :: L Token -> Loc
unpackTokLoc (L loc _) = loc

unpackCommentString :: L Token -> String
unpackCommentString (L _ (COMMENT s)) = s
unpackCommentString _ = error "unpackCommentString: not a comment"

isLocatedBefore :: L Token -> UncheckedDec -> Bool
isLocatedBefore comment dec = locOf dec > unpackTokLoc comment

fmt :: [UncheckedDec] -> [L Token] -> [Doc] -> [Doc]
fmt decs comments doc = do
  case decs of
    [] -> case comments of
      [] -> doc
      _ -> insertCommentAfter
    _ -> case comments of
      [] -> next
      _ ->
        if head comments `isLocatedBefore` head decs
          then insertCommentBefore
          else next
  where
    insertCommentAfter =
      fmt
        decs
        (tail comments)
        (doc ++ [text . unpackCommentString $ head comments])
    insertCommentBefore =
      fmt
        (tail decs)
        (tail comments)
        (doc ++ [string . unpackCommentString $ head comments] ++ [ppr $ head decs])
    next =
      fmt
        (tail decs)
        comments
        (doc ++ [ppr $ head decs])

formatSource :: [UncheckedDec] -> [L Token] -> Doc
formatSource decs comments = stack $ punctuate line $ fmt decs comments []

docstring :: String -> Doc
docstring "" = line
docstring ('\n' : s) = line <> text "-- " <> docstring s
docstring s = case span (/= '\n') s of
  (xs, ys) -> text xs <> docstring ys

formatDoc :: Maybe DocComment -> Doc
formatDoc m_dc =
  case m_dc of
    Nothing -> mempty
    Just (DocComment s loc) ->
      -- remove last newline to avoid misbehaviour
      if "\n" `isSuffixOf` s
        then do
          let s' = dropEnd 2 (pack s)
          text "-- | " <> docstring (unpack s')
        else text "-- | " <> docstring s

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
          -- (Right prog, comments) -> do
          -- TIO.putStrLn $ prettyText prog
          case prog of
            Prog doc decs -> do
              writeFile ("fmt." ++ file) $ PP.prettyCompact $ formatDoc doc <> formatSource decs comments -- write fmt to file
              -- putStrLn $ pretty $ ppr doc <> ppr' decs comments --write fmt to stdout
    _ -> Nothing