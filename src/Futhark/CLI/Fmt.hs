{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Prelude hiding (lines, unlines, writeFile, exp)
import Language.LSP.Types.Lens (HasDocumentChanges(documentChanges))
import Data.Maybe
import Control.Monad 
import Data.Loc (SrcLoc(SrcLoc))
import Data.Char (chr)
import Futhark.Util
import Data.Tuple

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
formatSource :: [UncheckedDec] -> [L Token] -> Doc
formatSource decs coms =
  stack $ map formatDec decs
  where
    formatDec dec = 
      case dec of
        ValDec vbb -> formatValBind vbb coms mempty
        _ -> text "NOT IMPLEMENTED YET"

    formatValBind (ValBind entry name retdecl rettype tparams args body docComment attrs sloc) coms' doc = do
      let vbb = ValBind entry name retdecl rettype tparams args body docComment attrs sloc
      if checkComment coms' sloc then do
        let tmp = doc <> (align . text . unpackCommentString $ head coms')
        formatValBind vbb (tail coms') tmp
      else do
        doc <> constructDocWithoutBody vbb </> indent 2 (constructExpDoc body coms')

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
    
    constructExpDoc exp coms' = do
      case exp of
        IntLit v _ sloc -> 
          if checkComment coms' sloc then do
            (align . text . unpackCommentString $ head coms') </> constructExpDoc exp (tail coms')
          else ppr v
        _ -> text "NOT IMPLEMENTED"

    restComments doc coms' = 
      case coms' of
        [] -> doc
        _ -> restComments (doc </> (text . unpackCommentString $ head coms')) (tail coms')

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
              writeFile ("fmt." ++ file) $ PP.pretty 80 $ prettyDocComment doc <> formatSource decs comments -- write fmt to file
              --putStrLn $ PP.pretty 80 $ prettyDocComment doc <> prettySource decs comments --write fmt to stdout
    _ -> Nothing