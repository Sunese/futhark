-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import Control.Monad
import Data.List (isSuffixOf)
import Data.Maybe
import Data.Text (dropEnd, pack, unpack)
import qualified Data.Text.IO as TIO
import Futhark.Util (trim)
import Futhark.Util.Loc hiding (SrcLoc)
import Futhark.Util.Options (mainWithOptions)
import Futhark.Util.Pretty
import Language.Futhark hiding (pretty)
import Language.Futhark.Parser (SyntaxError (syntaxErrorLoc, syntaxErrorMsg), parseWithComments)
import Language.Futhark.Parser.Lexer.Tokens
import System.IO (writeFile)
import qualified Text.PrettyPrint.Mainland as PP
import Prelude hiding (exp, lines, unlines, writeFile)

bodyOf :: ValBindBase f vn -> ExpBase f vn
bodyOf (ValBind _ _ _ _ _ _ body _ _ _) = body

unpackTokLoc :: L Token -> Loc
unpackTokLoc (L loc _) = loc

unpackCommentString :: L Token -> String
unpackCommentString (L _ (COMMENT s)) = s
unpackCommentString _ = error "unpackCommentString: not a comment"

isLocatedBefore :: L Token -> SrcLoc -> Bool
isLocatedBefore comment sl = locOf sl > unpackTokLoc comment

checkComment :: [L Token] -> SrcLoc -> Bool
checkComment coms' sloc' = 
  case coms' of 
    [] -> False
    _ -> head coms' `isLocatedBefore` sloc'

prettyDocComment :: Maybe DocComment -> Doc
prettyDocComment m_dc =
  case m_dc of
    Nothing -> mempty
    Just (DocComment s _) ->
      -- remove last newline to avoid misbehaviour
      if "\n" `isSuffixOf` s
        then do
          let s' = dropEnd 2 (pack s)
          text "-- | " <> docstring (unpack s')
        else text "-- | " <> docstring s

docstring :: String -> Doc
docstring "" = line
docstring ('\n' : s) = line <> text "-- " <> docstring s
docstring s = case span (/= '\n') s of
  (xs, ys) -> text xs <> docstring ys

-- TODO: how do we avoid next dec to re-print comments from last dec's area?
formatSource :: [UncheckedDec] -> [(Int, L Token)] -> Doc -> Doc
formatSource decs coms doc =
  case decs of
    [] -> 
      case coms of
        [] -> doc
        _ -> formatSource decs (tail coms) (doc </> (text . unpackCommentString $ head $ unzipComs coms))  --no more decs, but still some coms, consume rest
    _ -> 
      case coms of
        [] -> formatSource (tail decs) coms (doc <> ppr (head decs) </> line) -- still some decs, but no coms, ppr rest
        _ -> do
          let (decDoc, consumed) = formatDec $ head decs -- both coms and decs, run formatDec
          formatSource (tail decs) (drop consumed coms) (doc </> decDoc </> line)
  where
    -- check for comment here instead of inside each
    formatDec dec = 
      case dec of
        ValDec dec' -> formatValBind dec' coms mempty
        TypeDec dec' -> formatTypeBind dec' coms mempty
        SigDec sig -> (ppr sig, 0)
        ModDec sd -> (ppr sd, 0)
        OpenDec x _ -> (text "open" <+> ppr x, 0)
        LocalDec dec' _ -> (text "local" <+> ppr dec', 0)
        ImportDec x _ _ -> (text "import" <+> dquotes (ppr x), 0)

    unzipComs coms' = map snd coms'

    formatValBind dec zipComs doc' = do
      case zipComs of
        [] -> (doc' </> line <> constructDocWithoutBody dec </> indent 2 (ppr (bodyOf dec)), 10000)
        _ ->  if checkComment (unzipComs zipComs) (srclocOf dec) then do
                let tmp = doc' <> (align . text . unpackCommentString $ head (unzipComs zipComs))
                formatValBind dec (tail zipComs) tmp
              else do
                let (expDoc, consumed) = constructExpDoc (bodyOf dec) zipComs mempty
                (doc' </> line <> constructDocWithoutBody dec </> indent 2 expDoc, consumed)

    constructDocWithoutBody (ValBind entry name retdecl rettype tparams args _ docComment attrs _) = 
      mconcat (map ((<> line) . ppr) attrs)
        <> prettyDocComment docComment
        <> text fun
        <+> pprName name
        <+> align (sep (map ppr tparams ++ map ppr args))
        <> retdecl'
        <> text " = "
      where
        fun
          | isJust entry = "entry"
          | otherwise = "def"
        retdecl' = case (ppr <$> unAnnot rettype) `mplus` (ppr <$> retdecl) of
          Just rettype' -> colon <+> align rettype'
          Nothing -> mempty

    formatTypeBind (TypeBind name l params te rt docComment sloc) zipComs doc' = do
      case zipComs of
        [] -> 
          (prettyDocComment docComment <>
          text "type" <> ppr l <+> pprName name
          <+> spread (map ppr params)
          <+> equals
          <+> maybe (ppr te) ppr (unAnnot rt), 10000)
        _ -> 
          if checkComment (unzipComs zipComs) sloc then do
            let tmp = doc' <> (align . text . unpackCommentString $ head (unzipComs zipComs))
            formatTypeBind (TypeBind name l params te rt docComment sloc) (tail zipComs) tmp
          else do
            let (expDoc, consumed) = constructExpDoc te zipComs mempty
            (prettyDocComment docComment <>
              text "type" <> ppr l <+> pprName name
              <+> spread (map ppr params)
              <+> equals
              </> indent 2 expDoc, consumed)

    constructExpDoc exp zipComs expDoc =
      case zipComs of 
        [] -> (expDoc </> ppr exp, 10000)
        _ -> if checkComment (unzipComs zipComs) (srclocOf exp) then do
          let tmpDoc = expDoc </> (align . text . unpackCommentString $ head $ unzipComs zipComs)
          constructExpDoc exp (tail zipComs) tmpDoc
        else (expDoc </> ppr exp, fst $ head zipComs)

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
          case prog of
            Prog doc decs -> do
              --print $ head decs
              writeFile ("fmt." ++ file) 
                $ trim
                $ PP.pretty 80 
                $ prettyDocComment doc 
                  <> formatSource decs (zip [0..] comments) mempty -- write fmt to file
              --putStrLn $ PP.pretty 80 $ prettyDocComment doc <> prettySource decs comments --write fmt to stdout
    _ -> Nothing