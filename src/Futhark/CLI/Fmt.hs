-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import CMarkGFM (PosInfo (startColumn))
import Control.Monad
import Data.List (isSuffixOf)
import Data.Loc
import Data.Maybe
import Data.Text (cons, dropEnd, pack, unpack)
import qualified Data.Text.IO as TIO
import Futhark.Util (trim)
import Futhark.Util.Loc hiding (SrcLoc)
import Futhark.Util.Options (mainWithOptions)
import Futhark.Util.Pretty (srcloc)
import Futhark.Util.Pretty hiding (srcloc)
import Language.Futhark hiding (Comment (..), pretty)
import Language.Futhark.Parser (SyntaxError (syntaxErrorLoc, syntaxErrorMsg), parseWithComments)
import Language.Futhark.Parser.Lexer.Tokens
import System.IO (writeFile)
import qualified Text.PrettyPrint.Mainland as PP
import qualified Text.PrettyPrint.Mainland as TIO
import Prelude hiding (exp, lines, unlines, writeFile)

-- | Modes for rendering of pending comments.
data CommentPosition
  = -- | Put the comment on the same line
    OnTheSameLine
  | -- | Put the comment on next line
    OnNextLine
  deriving (Eq, Show)

-- TODO: commentloc and commentposition not used?
data Comment = Comment
  { commentString :: String,
    commentLoc :: Loc,
    commentPosition :: CommentPosition
  }

hasArrayLit :: ExpBase ty vn -> Bool
hasArrayLit ArrayLit {} = True
hasArrayLit (TupLit es2 _) = any hasArrayLit es2
hasArrayLit _ = False

dropEnd1 :: [a] -> [a]
dropEnd1 [] = []
dropEnd1 (x : xs) = foldr (\z f y -> y : f z) (const []) xs x

bodyOf :: ValBindBase f vn -> ExpBase f vn
bodyOf (ValBind _ _ _ _ _ _ body _ _ _) = body

startLineOfComment :: Comment -> Int
startLineOfComment (Comment _ NoLoc _) = -1
startLineOfComment (Comment _ (Loc pos _) _) =
  case pos of
    Pos _ line' _ _ -> line'

startLineOfSrcLoc :: SrcLoc -> Int
startLineOfSrcLoc (SrcLoc l) =
  case l of
    NoLoc -> -1
    Loc pos _ -> case pos of Pos _ line' _ _ -> line'

endColOfSrcLoc :: SrcLoc -> Int
endColOfSrcLoc (SrcLoc l) =
  case l of
    NoLoc -> -1
    Loc _ pos -> case pos of Pos _ _ col _ -> col

endColOfLoc :: Loc -> Int
endColOfLoc (Loc _ pos) =
  case pos of Pos _ _ col _ -> col
endColOfLoc NoLoc = -1

startColOfLoc :: Loc -> Int
startColOfLoc (Loc pos _) =
  case pos of Pos _ _ col _ -> col
startColOfLoc NoLoc = -1

startLineOfLoc :: Loc -> Int
startLineOfLoc (Loc pos _) =
  case pos of
    Pos _ line' _ _ -> line'
startLineOfLoc _ = -1

unpackTokSrcLoc :: L Token -> Loc
unpackTokSrcLoc (L loc _) = loc

unpackComTokString :: L Token -> String
unpackComTokString (L _ (COMMENT s)) = s
unpackComTokString _ = error "unpackCommentString: not a comment"

unpackCommentString :: Comment -> String
unpackCommentString = commentString

isLocatedBefore :: Comment -> SrcLoc -> Bool
isLocatedBefore (Comment _ loc _) sl = locOf sl > loc

unzipComs :: [(Int, Comment)] -> [Comment]
unzipComs = map snd

checkZipComment :: [(Int, Comment)] -> SrcLoc -> Bool
checkZipComment coms' sloc' = head (unzipComs coms') `isLocatedBefore` sloc'

checkComment :: [Comment] -> SrcLoc -> Bool
checkComment coms' sloc' = head coms' `isLocatedBefore` sloc'

prettyDocComment :: Maybe DocComment -> Doc
prettyDocComment m_dc =
  case m_dc of
    Nothing -> mempty
    Just (DocComment s _) ->
      -- remove last newline to avoid misbehaviour
      -- TODO: avoid dropEnd
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

toBeDropped :: [(Int, Comment)] -> Int
toBeDropped coms = fst (head coms) - 1

commentsBefore :: [(Int, Comment)] -> SrcLoc -> SrcLoc -> [Doc] -> Bool -> ([Doc], Int, Bool)
commentsBefore comments sloc lastSrcLoc tmpDocs inserted =
  case comments of
    [] -> (tmpDocs, 10000, inserted)
    _ ->
      if checkZipComment comments sloc
        then
          commentsBefore
            (tail comments)
            sloc
            lastSrcLoc
            (insertComment (snd $ head comments) lastSrcLoc tmpDocs)
            True
        else
          if inserted
            then (tmpDocs, toBeDropped comments, inserted)
            else (tmpDocs, 0, inserted)

insertComment :: Comment -> SrcLoc -> [Doc] -> [Doc]
insertComment (Comment commentstring commentloc _) sloc docs =
  if startLineOfSrcLoc sloc == startLineOfSrcLoc (srclocOf commentloc)
    then
      let doc = last docs
          doc' = doc <+> text commentstring
       in dropEnd1 docs ++ [doc']
    else docs ++ [text commentstring]

prepareComments :: [L Token] -> [Comment] -> [Comment]
prepareComments tokens coms =
  case tokens of
    [] -> coms
    _ -> prepareComments (tail tokens) (coms ++ [Comment (unpackComTokString $ head tokens) (unpackTokSrcLoc $ head tokens) OnNextLine])

changeCommentPos :: Comment -> Comment
changeCommentPos (Comment string srcloc position) =
  if position == OnNextLine
    then Comment string srcloc OnTheSameLine
    else Comment string srcloc position

formatSource :: [UncheckedDec] -> [Comment] -> [Doc] -> SrcLoc -> Doc
formatSource decs coms tmpDoc lastSrcLoc =
  case decs of
    [] ->
      case coms of
        [] -> stack tmpDoc
        _ -> formatSource decs (tail coms) (insertComment (head coms) lastSrcLoc tmpDoc) lastSrcLoc
    _ ->
      case coms of
        [] -> do
          let (decDoc, consumed, lastSrcLoc') = formatDec (head decs) [] tmpDoc lastSrcLoc
          formatSource (tail decs) [] decDoc lastSrcLoc'
        _ -> do
          let (decDoc, consumed, lastSrcLoc') = formatDec (head decs) coms tmpDoc lastSrcLoc
          formatSource (tail decs) (drop consumed coms) decDoc lastSrcLoc'

-- stack $ tmpDoc ++ decDoc

formatDec :: UncheckedDec -> [Comment] -> [Doc] -> SrcLoc -> ([Doc], Int, SrcLoc)
formatDec dec coms tmpDoc lastsrcloc = do
  -- Check if there was a comment before new declaration
  let zipComs = zip [1 ..] coms
  let (tmpDoc', consumed, inserted) = commentsBefore zipComs (srclocOf dec) lastsrcloc tmpDoc False
  let tmpDoc'' = tmpDoc'
  case dec of
    ValDec dec' -> do
      formatValBind dec' (drop consumed zipComs) tmpDoc''
    TypeDec dec' -> do
      (tmpDoc'' ++ [ppr dec'], consumed, srclocOf dec')
    SigDec sig -> (tmpDoc'' ++ [ppr sig], consumed, srclocOf sig)
    ModDec sd ->
      (tmpDoc'' ++ [ppr sd], consumed, srclocOf sd)
    OpenDec x _ -> (tmpDoc'' ++ [text "open" <+> ppr x], consumed, srclocOf x)
    LocalDec dec' sloc -> do
      (tmpDoc'' ++ [text "local" <+> ppr dec'], consumed, sloc)
    ImportDec x _ _ -> (tmpDoc'' ++ [text "import" <+> dquotes (ppr x)], consumed, srclocOf dec)

formatValBind :: (Eq vn, IsName vn, Annot f) => ValBindBase f vn -> [(Int, Comment)] -> [Doc] -> ([Doc], Int, SrcLoc)
formatValBind dec zipComs topLevelDoc = do
  let (bodyDoc, consumed, lastSrcLoc) = formatExpBase (bodyOf dec) zipComs [] $ srclocOf dec -- give it empty Doc because top level has already been checked
  (constructDocWithoutBody dec bodyDoc, consumed, lastSrcLoc)
  where
    constructDocWithoutBody :: (Eq vn, IsName vn, Annot f) => ValBindBase f vn -> [Doc] -> [Doc]
    constructDocWithoutBody (ValBind entry name retdecl rettype tparams args _ docCom attrs _) bodyDoc =
      topLevelDoc
        ++ mconcat (map ((<> line) . ppr) attrs)
        <> prettyDocComment docCom
        <> text fun
        <+> pprName name
        <+> align maybeSplitUpArgs
        <> retdecl'
        <> text " =" :
      map indentation bodyDoc
      where
        fun
          | isJust entry = "entry"
          | otherwise = "def"
        retdecl' = case (ppr <$> unAnnot rettype) `mplus` (ppr <$> retdecl) of
          Just rettype' -> colon <+> align rettype'
          Nothing -> mempty
        maybeSplitUpArgs =
          if totalLengthOfArgs < 80
            then sep (map ppr tparams ++ map ppr args)
            else line <> stack (map (indent 2 . ppr) tparams ++ map (indent 2 . ppr) args)
        indentation = if totalLengthOfArgs < 80 then indent 2 else indent 4
        totalLengthOfArgs = sum (map lengthof args) + length args * 2 + length (prettyName name) + 4
          where
            lengthof arg = endColOfLoc (locOf arg) - startColOfLoc (locOf arg)

formatExpBase :: (Eq vn, IsName vn, Annot f) => ExpBase f vn -> [(Int, Comment)] -> [Doc] -> SrcLoc -> ([Doc], Int, SrcLoc)
formatExpBase exp zipComs tmpDoc lastSrcLoc = do
  let (docBeforeExp, consumed, commentinserted) = commentsBefore zipComs (srclocOf exp) lastSrcLoc tmpDoc False
  case exp of
    AppExp e _ -> do
      case e of
        LetPat sizes pat e' body sloc -> do
          let (letDoc, _, lastsrcloc) = formatExpBase e' [] [] sloc -- dont pass coms or doc along because we already checked for comments before this let
          let expDocWithoutBody = firstpat -- contains everything before the body of the let (also comments)
                where
                  linebreak = case e' of
                    AppExp {} -> True
                    Attr {} -> True
                    ArrayLit {} -> False
                    _ -> hasArrayLit e'
                  firstpat =
                    if linebreak
                      then docBeforeExp ++ align (text "let" <+> spread (map ppr sizes) <+> align (ppr pat) <+> equals) : map (indent 2) letDoc
                      else do
                        docBeforeExp ++ align (text "let" <+> spread (map ppr sizes) <+> align (ppr pat) <+> equals <+> align (head letDoc)) : tail letDoc
          let (bodyDoc, consumedAfterBody, srcLocOfLastLine) = letBody body (drop consumed zipComs) expDocWithoutBody lastsrcloc
          (bodyDoc, consumedAfterBody, srcLocOfLastLine)
        If c t f sloc -> do
          let ifthenDoc = docBeforeExp ++ [text "if" <+> ppr c <+> text "then" <+> ppr t <+> text "else"]
          let (elsebody, consumed', lastSrcLoc') = formatExpBase f (drop consumed zipComs) ifthenDoc sloc
          (elsebody, consumed', lastSrcLoc')
        _ -> do
          (docBeforeExp ++ [ppr exp], consumed, srclocOf e)
    _ ->
      (docBeforeExp ++ [ppr exp], consumed, srclocOf exp)

letBody :: (Eq vn, IsName vn, Annot f) => ExpBase f vn -> [(Int, Comment)] -> [Doc] -> SrcLoc -> ([Doc], Int, SrcLoc)
letBody body@(AppExp LetPat {} _) zipComs tmpDoc srcLocOfLet = do
  let (docandcommentsbefore, consumed, _) = commentsBefore zipComs (srclocOf body) srcLocOfLet tmpDoc False
  formatExpBase body (drop consumed zipComs) docandcommentsbefore srcLocOfLet

letBody body@(AppExp LetFun {} _) zipComs tmpDoc srcLocOfLet = do
  let (docandcommentsbefore, consumed, _) = commentsBefore zipComs (srclocOf body) srcLocOfLet tmpDoc False
  formatExpBase body (drop consumed zipComs) docandcommentsbefore srcLocOfLet

letBody body zipComs tmpDoc srcLocOfLet = do
  let doc = last tmpDoc
  let doc' = doc <+> text "in"
  let (doc'', consumed, lastsrcloc) = formatExpBase body zipComs (dropEnd1 tmpDoc ++ [doc']) srcLocOfLet
  (doc'', consumed, lastsrcloc)

-- (commentsBeforeBody, consumed, srcLocOfLet)

formatTypeBind :: TypeBindBase NoInfo Name -> [(Int, Comment)] -> [Doc] -> ([Doc], Int, SrcLoc)
formatTypeBind (TypeBind name l params te _ _ sloc) zipComs tmpDoc = do
  let doc =
        [ text "type" <> ppr l <+> pprName name
            <+> spread (map ppr params)
            <+> equals
            <+> ppr te
        ]
  case zipComs of
    [] -> (tmpDoc ++ doc, 0, sloc)
    _ -> (tmpDoc ++ doc, toBeDropped zipComs, sloc)

constructTypeExpDoc :: TypeExp Name -> [(Int, Comment)] -> [Doc] -> SrcLoc -> ([Doc], Int, SrcLoc)
constructTypeExpDoc exp zipComs tmpDoc lastSrcLoc = do
  let (docBefore, consumed, commentinserted) = commentsBefore zipComs (srclocOf exp) lastSrcLoc tmpDoc False
  let docBefore' = dropEnd1 docBefore ++ [last docBefore <+> ppr exp]
  (docBefore', consumed, srclocOf exp)

-- | Run @futhark fmt@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      s <- TIO.readFile file
      -- print file
      case parseWithComments file s of
        (Left e, _) -> do
          putStrLn $ "Syntax error. File: " ++ file ++ locStr (syntaxErrorLoc e)
        (Right prog1, comments1) -> do
          case prog1 of
            Prog doc decs -> do
              let formattedText1 =
                    pack $
                      trim $
                        PP.prettyCompact $
                          prettyDocComment doc
                            </> formatSource
                              decs
                              (prepareComments comments1 [])
                              []
                              (srclocOf $ last decs)
              -- TIO.writeFile ("fmt." ++ file) formattedText1
              case parseWithComments file formattedText1 of
                (Left e, _) -> do
                  -- putStrLn $ "The formatted file contains a synax error. File: " ++ file ++ locStr (syntaxErrorLoc e)
                  mempty
                (Right prog2, comments2) -> do
                  let prog1str = show prog1
                  let prog2str = show prog2
                  -- AST check
                  if prog1str == prog2str
                    then case prog2 of
                      Prog doc2 decs2 -> do
                        let formattedText2 =
                              pack $
                                trim $
                                  PP.prettyCompact $
                                    prettyDocComment doc2
                                      </> formatSource
                                        decs2
                                        (prepareComments comments2 [])
                                        []
                                        (srclocOf $ last decs2)
                        -- idempotence check
                        if formattedText1 == formattedText2
                          then do
                            -- TIO.writeFile file formattedText2
                            putStrLn $ "All done! File has been formatted: " ++ file
                            mempty
                          else do
                            -- TIO.writeFile ("fmt1." ++ file) formattedText1
                            -- TIO.writeFile ("fmt2." ++ file) formattedText2
                            -- writeFile ("AST1." ++ file) prog1str
                            -- writeFile ("AST2." ++ file) prog2str
                            putStrLn $ "Formatter is not idempotent, please report this. File: " ++ file
                            mempty
                    else do
                      putStrLn $ "ASTs differ, please report this. File: " ++ file
                      -- writeFile ("AST1." ++ file) prog1str
                      -- writeFile ("AST2." ++ file) prog2str
                      mempty
    _ -> Nothing