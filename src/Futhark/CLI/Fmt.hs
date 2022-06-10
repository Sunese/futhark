-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import CMarkGFM (PosInfo (startColumn))
import Control.Monad
import Data.List (isSuffixOf)
import Data.Loc
import Data.Maybe
import Data.Text (dropEnd, pack, unpack)
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

checkComment :: [(Int, Comment)] -> SrcLoc -> Bool
checkComment coms' sloc' = head (unzipComs coms') `isLocatedBefore` sloc'

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

commentsBefore :: [(Int, Comment)] -> SrcLoc -> SrcLoc -> [Doc] -> Bool -> ([Doc], Int, Bool)
commentsBefore comments sloc lastSrcLoc tmpDocs inserted =
  case comments of
    [] -> (tmpDocs, 10000, inserted)
    _ ->
      if checkComment comments sloc
        then
          commentsBefore
            (tail comments)
            sloc
            lastSrcLoc
            (insertComment (snd $ head comments) lastSrcLoc tmpDocs)
            True
        else
          if inserted
            then (tmpDocs, fst $ head comments, inserted)
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
          let (decDoc, consumed, lastSrcLoc') = formatDec (head decs) [] mempty
          formatSource (tail decs) [] (tmpDoc ++ decDoc) lastSrcLoc'
        _ -> do
          if head coms `isLocatedBefore` srclocOf (head decs)
            then formatSource decs (tail coms) (tmpDoc ++ [text . unpackCommentString $ head coms]) lastSrcLoc
            else do
              --stack $ map (text . unpackCommentString) coms
              let (decDoc, consumed, lastSrcLoc') = formatDec (head decs) (zip [1 ..] coms) tmpDoc
              formatSource (tail decs) (drop consumed coms) (tmpDoc ++ decDoc) lastSrcLoc'
              -- stack $ tmpDoc ++ decDoc

formatDec :: UncheckedDec -> [(Int, Comment)] -> [Doc] -> ([Doc], Int, SrcLoc)
formatDec dec zipComs tmpDoc =
  case dec of
    ValDec dec' -> do
      let (doc, consumed, sloc) = formatValBind dec' zipComs tmpDoc
      (doc ++ [text ""], consumed, sloc)
    TypeDec dec' -> do
      let (doc, consumed, sloc) = formatTypeBind dec' zipComs tmpDoc
      (doc ++ [text ""], consumed, sloc)
    SigDec sig -> ([ppr sig], 0, srclocOf sig)
    ModDec sd -> ([ppr sd], 0, srclocOf sd)
    OpenDec x _ -> ([text "open" <+> ppr x], 0, srclocOf x)
    LocalDec dec' _ -> do
      let (doc', consumed, lastSrcLoc) = formatDec dec' zipComs tmpDoc
      (text "local" <+> head doc' : tail doc', consumed, lastSrcLoc)
    ImportDec x _ _ -> ([text "import" <+> dquotes (ppr x)], 0, srclocOf dec)

formatValBind :: (Eq vn, IsName vn, Annot f) => ValBindBase f vn -> [(Int, Comment)] -> [Doc] -> ([Doc], Int, SrcLoc)
formatValBind dec zipComs tmpDoc = do
  let (expDoc, consumed, lastSrcLoc) = formatExpBase (bodyOf dec) zipComs tmpDoc $ srclocOf dec
  (constructDocWithoutBody dec expDoc, consumed, lastSrcLoc)
  where
    constructDocWithoutBody :: (Eq vn, IsName vn, Annot f) => ValBindBase f vn -> [Doc] -> [Doc]
    constructDocWithoutBody (ValBind entry name retdecl rettype tparams args _ docCom attrs _) expDoc =
      mconcat (map ((<> line) . ppr) attrs)
        <> prettyDocComment docCom
        <> text fun
        <+> pprName name
        <+> align maybeSplitUpArgs
        <> retdecl'
        <> text " = " :
      map indentation expDoc
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
    Parens e srcloc' -> 
      if commentinserted then 
      (docBeforeExp ++ [align $ parens $ ppr exp], consumed, srcloc')
      else ([align $ parens $ ppr exp], consumed, srcloc')
    AppExp e _ -> do
      case e of
        DoLoop _ _ _ _ _ sloc -> do
          let (comsDoc', consumed', commentinserted) = commentsBefore (drop consumed zipComs) sloc lastSrcLoc docBeforeExp False
          (docBeforeExp ++ comsDoc' ++ [ppr exp], consumed', srclocOf e)
        LetPat sizes pat e' body srcloc' -> do
          let expDocWithoutBody = firstpat
                where
                  linebreak = case e' of
                    AppExp {} -> True
                    Attr {} -> True
                    ArrayLit {} -> False
                    _ -> hasArrayLit e'
                  firstpat =
                    if linebreak
                      then align (text "let" <+> spread (map ppr sizes) <+> align (ppr pat) <+> equals) : [indent 2 (ppr e')]
                      else [align $ text "let" <+> spread (map ppr sizes) <+> align (ppr pat) <+> equals <+> align (ppr e')]
          let (bodyDoc, consumedAfterBody, srcLocOfLastLine) = letBody body (drop consumed zipComs) expDocWithoutBody srcloc'
          if consumed > 0 || consumed == 10000 then
            (docBeforeExp ++ bodyDoc, consumedAfterBody, srcLocOfLastLine)
          else (bodyDoc, consumedAfterBody, srcLocOfLastLine)
        If c t f sloc -> do
          case f of
            AppExp _ _ -> do -- multiple lines, split it up
              let ifthenDoc = [text "if" <+> ppr c <+> text "then" <+> ppr t]
              let (docbeforeelse, consumed'', commentinserted) = commentsBefore (drop consumed zipComs) (srclocOf f) sloc ifthenDoc False

              let (elsebody, consumed''', lastSrcLoc') = formatExpBase f (drop consumed'' zipComs) docbeforeelse sloc
              let elsebody' = (text "else" <+> head elsebody) : tail elsebody
              (docbeforeelse ++ elsebody', consumed''', lastSrcLoc')

            _ -> do -- probably an atom, just ppr it
              let doc = (text "if" <+> ppr c <+> text "then" <+> ppr t) : [text "else" <+> ppr f]
              (doc, consumed, srclocOf f)
        _ -> do
          let (commentsBeforeExp, consumed', commentinserted') = commentsBefore (drop consumed zipComs) (srclocOf e) lastSrcLoc docBeforeExp False
          --(commentsBeforeExp ++ [ppr exp], consumed', srclocOf e)
          --(docBeforeExp, consumed', srclocOf e)
          if commentinserted || commentinserted'
            then (commentsBeforeExp ++ [ppr exp], consumed', srclocOf e)
            else ([ppr exp], consumed', srclocOf e)
    _ ->
      if commentinserted then
        (docBeforeExp ++ [ppr exp], consumed, srclocOf exp)
      else
        ([ppr exp], consumed, srclocOf exp)

letBody :: (Eq vn, IsName vn, Annot f) => ExpBase f vn -> [(Int, Comment)] -> [Doc] -> SrcLoc -> ([Doc], Int, SrcLoc)
letBody body@(AppExp LetPat {} _) zipComs tmpDoc srcLocOfBody = formatExpBase body zipComs tmpDoc srcLocOfBody
letBody body@(AppExp LetFun {} _) zipComs tmpDoc srcLocOfBody = formatExpBase body zipComs tmpDoc srcLocOfBody
letBody body zipComs tmpDoc srcLocOfBody = do
  let (commentsBeforeBody, consumed, commentinserted) = commentsBefore zipComs (srclocOf body) srcLocOfBody tmpDoc False
  let bodyDoc = [text "in" <+> align (ppr body)]
  (commentsBeforeBody ++ bodyDoc, consumed, srclocOf body)

formatTypeBind :: TypeBindBase NoInfo Name -> [(Int, Comment)] -> [Doc] -> ([Doc], Int, SrcLoc)
formatTypeBind (TypeBind name l params te _ _ sloc) _ _ = do
  let doc = [text "type" <> ppr l <+> pprName name
        <+> spread (map ppr params)
        <+> equals <+> ppr te]
  (doc, 0, sloc)

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
      --print file
      case parseWithComments file s of
        (Left e, _) -> do
          putStr $ "Syntax error. File: " ++ file ++ locStr (syntaxErrorLoc e) ++ "\n" ++ syntaxErrorMsg e ++ "\n"
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
              TIO.writeFile ("fmt." ++ file) formattedText1
              case parseWithComments file formattedText1 of
                (Left e, _) -> do
                  putStr $ "The formatted file contains a synax error. File: " ++ file ++ locStr (syntaxErrorLoc e) ++ "\n" ++ syntaxErrorMsg e ++ "\n"
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
                            --TIO.writeFile file formattedText2
                            putStrLn $ "All done! File has been formatted: " ++ file
                            mempty
                          else do
                            TIO.writeFile ("fmt1." ++ file) formattedText1
                            TIO.writeFile ("fmt2." ++ file) formattedText2
                            writeFile ("AST1." ++ file) prog1str
                            writeFile ("AST2." ++ file) prog2str
                            putStrLn $ "Formatter is not idempotent, please report this. File: " ++ file ++ "\n"
                    else do 
                      putStrLn $ "ASTs differ, please report this. File: " ++ file ++ "\n"
                      writeFile ("AST1." ++ file) prog1str
                      writeFile ("AST2." ++ file) prog2str
                      mempty
    _ -> Nothing