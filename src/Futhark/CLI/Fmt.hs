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

formatSource :: [UncheckedDec] -> [L Token] -> [Doc] -> Doc
formatSource decs coms doc =
  case decs of 
    [] -> 
      case coms of
        [] -> stack doc
        _ -> formatSource decs (tail coms) (doc ++ [text . unpackCommentString $ head coms])
    _ ->
      case coms of 
        [] -> formatSource (tail decs) coms (doc ++ [ppr $ head decs])
        _ -> do
          let numberedComs = zip [0..] coms
          let (decDoc, consumed) = formatDec (head decs) doc numberedComs
          formatSource (tail decs) (drop consumed coms) (doc ++ decDoc)

formatDec :: UncheckedDec -> [Doc] -> [(Int, L Token)] -> ([Doc], Int)
formatDec dec doc numberedComs  = case dec of
  ValDec vbb -> formatValBind vbb doc numberedComs 0
  _ -> ([text "THIS SHOULD NOT APPEAR"], 0)

formatValBind :: UncheckedValBind -> [Doc] -> [(Int, L Token)] -> Int -> ([Doc], Int)
formatValBind (ValBind entry name retdecl rettype tparams args body docComment attrs sloc) doc numberedComs consumed =
  case numberedComs of 
    [] -> (doc ++ [constructDocWithoutBody </> indent 2 (ppr body)], consumed)
    _ -> do
      if checkComment (map snd numberedComs) sloc then do
        let tmpDoc = doc ++ [align . text . unpackCommentString $ snd (head numberedComs)]
        formatValBind 
          (ValBind entry name retdecl rettype tparams args body docComment attrs sloc)
          tmpDoc 
          (tail numberedComs)
          (fst $ head numberedComs)
      else do
        let (expDoc, consumed') = formatExp body numberedComs []
        (doc ++ [constructDocWithoutBody </> indent 2 expDoc], consumed')
  where
    fun
      | isJust entry = "entry"
      | otherwise = "def"
    retdecl' = case (ppr <$> unAnnot rettype) `mplus` (ppr <$> retdecl) of
      Just rettype' -> colon <+> align rettype'
      Nothing -> mempty
    constructDocWithoutBody = 
      mconcat (map ((<> line) . ppr) attrs)
            <> prettyDocComment docComment
            <> text fun
            <+> pprName name
            <+> align (sep (map ppr tparams ++ map ppr args))
            <> retdecl'
            <> text " ="

formatExp :: UncheckedExp -> [(Int, L Token)] -> [Doc] -> (Doc, Int)
formatExp exp numberedComs expDoc = do 
  case exp of
    Literal v sl -> formatLit v sl 
    IntLit v _ sl -> formatLit v sl 
    _ -> (text "THIS SHOULD NOT APPEAR", 0)
  where 
    formatLit v sl = 
      case numberedComs of
        [] -> (stack $ expDoc ++ [ppr v], 1000)
        _ -> 
          if checkComment (map snd numberedComs) sl then do
            let tmpdoc = expDoc ++ [align . text . unpackCommentString $ head $ map snd numberedComs]
            formatExp exp (tail numberedComs) tmpdoc
          else (stack $ expDoc ++ [ppr v], fst (head numberedComs))

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
              writeFile ("fmt." ++ file) $ PP.pretty 80 $ prettyDocComment doc <> formatSource decs comments [] -- write fmt to file
              --putStrLn $ PP.pretty 80 $ prettyDocComment doc <> prettySource decs comments --write fmt to stdout
    _ -> Nothing


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


-- inserts comment on line above and pprs exp
checkComment' :: Pretty a => SrcLoc -> [L Token] -> a -> Doc
checkComment' loc comments v  =
  if head comments `isLocatedBefore` loc then
    (string . unpackCommentString $ head comments) </> ppr v
  else
    ppr v 


prettyExp' :: ExpBase NoInfo Name -> [L Token] -> Doc
prettyExp' e comments = case e of
  Literal v loc -> case comments of
    [] -> ppr v
    _ -> checkComment' loc comments v
  IntLit v _ loc -> case comments of
    [] -> ppr v
    _ -> if head comments `isLocatedBefore` loc then 
      (string . unpackCommentString $ head comments) <> line <> prettyExp' e (tail comments)
      else ppr v
  FloatLit v _ loc -> checkComment' loc comments v <> ppr v
  StringLit s loc -> checkComment' loc comments s <> text (show $ map (chr . fromIntegral) s)
  Hole ni loc -> mempty -- TODO: support for hole
  Var name t loc -> checkComment' loc comments name <> ppr name <> inst
    where
      inst = case unAnnot t of
        Just t'
          | isEnvVarAtLeast "FUTHARK_COMPILER_DEBUGGING" 2 ->
              text "@" <> parens (align $ ppr t')
        _ -> mempty
  Parens e loc -> ppr e
  QualParens (v, _) e loc -> ppr e
  TupLit es loc -> ppr e
  RecordLit fs loc -> ppr e
  ArrayLit es info loc -> ppr e
  Attr attr e loc -> ppr e
  Project k e _ loc -> ppr e
  Negate e loc -> ppr e
  Not e loc -> ppr e
  Assert e1 e2 _ loc -> ppr e
  Constr n cs _ loc -> ppr e
  Update src idxs ve loc -> ppr e
  RecordUpdate src fs ve _ loc -> ppr e
  Lambda params body rettype _ loc -> ppr e
  OpSection binop _ loc -> ppr e
  OpSectionLeft binop _ x _ _ loc -> ppr e
  OpSectionRight binop _ x _ _ loc -> ppr e
  ProjectSection fields _ loc -> ppr e
  IndexSection idxs _ loc -> ppr e
  Ascript e t loc -> ppr e
  AppExp e loc -> ppr e