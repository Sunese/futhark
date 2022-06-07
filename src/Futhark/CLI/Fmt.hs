-- | @futhark fmt@
module Futhark.CLI.Fmt (main) where

import Control.Monad
import Data.List (isSuffixOf)
import Data.Loc
import Data.Maybe
import Data.Text (dropEnd, pack, unpack)
import qualified Data.Text.IO as TIO
import Futhark.Util (trim)
import Futhark.Util.Loc hiding (SrcLoc)
import Futhark.Util.Options (mainWithOptions)
import Futhark.Util.Pretty hiding (srcloc)
import Language.Futhark hiding (pretty, Comment(..))
import Language.Futhark.Parser (SyntaxError (syntaxErrorLoc, syntaxErrorMsg), parseWithComments)
import Language.Futhark.Parser.Lexer.Tokens
import System.IO (writeFile)
import qualified Text.PrettyPrint.Mainland as PP
import Prelude hiding (exp, lines, unlines, writeFile)
import Futhark.Util.Pretty (srcloc)
import CMarkGFM (PosInfo(startColumn))

-- | Modes for rendering of pending comments.
data CommentPosition
  = -- | Put the comment on the same line
    OnTheSameLine
  | -- | Put the comment on next line
    OnNextLine
  deriving (Eq, Show)

data Comment = Comment  {
  commentString :: String,
  commentLoc :: Loc,
  commentPosition :: CommentPosition
}

hasArrayLit :: ExpBase ty vn -> Bool
hasArrayLit ArrayLit {} = True
hasArrayLit (TupLit es2 _) = any hasArrayLit es2
hasArrayLit _ = False

dropEnd1 :: [a] -> [a]
dropEnd1 [] = []
dropEnd1 (x:xs) = foldr (\z f y -> y : f z) (const []) xs x

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

commentsBefore :: [(Int, Comment)] -> SrcLoc -> SrcLoc -> [Doc] -> Bool -> ([Doc], Int)
commentsBefore comments sloc lastSrcLoc tmpDocs inserted = 
  case comments of 
    [] -> (tmpDocs, 10000)
    _ -> 
      if checkComment comments sloc
        then 
          commentsBefore
              (tail comments)
              sloc 
              lastSrcLoc
              (insertComment (snd $ head comments) lastSrcLoc tmpDocs) -- will insert comment at the right place
              True
      else -- no comment before, but still some left - figure out if some were inserted and needs to be dropped later
        if inserted then (tmpDocs, fst $ head comments)
        else (tmpDocs, 0)

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

insertComment :: Comment -> SrcLoc -> [Doc] -> [Doc]
insertComment (Comment commentstring commentloc _) sloc docs = 
  if startLineOfSrcLoc sloc == startLineOfSrcLoc (srclocOf commentloc) then
    -- append to doc at last entry of [Doc]
    let doc = last docs
        doc' = doc <+> text commentstring
    in dropEnd1 docs ++ [doc']
  else
    -- otherwise put comment on new line
    docs ++ [text commentstring]

formatSource :: [UncheckedDec] -> [Comment] -> [Doc] -> SrcLoc -> Doc
formatSource decs coms tmpDoc lastSrcLoc =
  case decs of
    [] -> 
      case coms of
        [] -> stack tmpDoc -- we are done
        _ -> formatSource decs (tail coms) (insertComment (head coms) lastSrcLoc tmpDoc) lastSrcLoc
          --formatSource decs (tail coms) (docs ++ [text . unpackCommentString $ head coms])  --no more decs, but still some coms, consume rest
    _ -> 
      case coms of
        [] -> let (decDoc, consumed, lastSrcLoc') = formatDec (head decs) [] tmpDoc
              in formatSource (tail decs) [] (tmpDoc ++ decDoc) lastSrcLoc'
           --formatSource (tail decs) coms (tmpDoc ++ [ppr (head decs)]) lastSrcLoc -- still some decs, but no coms, ppr rest
        _ -> do
          if head coms `isLocatedBefore` srclocOf (head decs) then
            formatSource decs (tail coms) (tmpDoc ++ [text . unpackCommentString $ head coms]) lastSrcLoc
          else do
            let (decDoc, consumed, lastSrcLoc') = formatDec (head decs) (zip [1..] coms) tmpDoc
            formatSource (tail decs) (drop consumed coms) (tmpDoc ++ decDoc) lastSrcLoc'

formatDec :: UncheckedDec -> [(Int, Comment)] -> [Doc] -> ([Doc], Int, SrcLoc)
formatDec dec zipComs tmpDoc = 
  case dec of
    ValDec dec' -> formatValBind dec' zipComs tmpDoc
    TypeDec dec' -> formatTypeBind dec' zipComs
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
        <> text " = " : map indentation expDoc
      where
        fun
          | isJust entry = "entry"
          | otherwise = "def"
        retdecl' = case (ppr <$> unAnnot rettype) `mplus` (ppr <$> retdecl) of
          Just rettype' -> colon <+> align rettype'
          Nothing -> mempty
        maybeSplitUpArgs = if totalLengthOfArgs < 80
                        then sep (map ppr tparams ++ map ppr args)
                        else line <> stack (map (indent 2 . ppr) tparams ++ map (indent 2 . ppr) args)
        indentation = if totalLengthOfArgs < 80 then indent 2 else indent 4
        totalLengthOfArgs = sum (map lengthof args) + length args * 2 + length (prettyName name) + 4
          where
            lengthof arg = endColOfLoc (locOf arg) - startColOfLoc (locOf arg)

formatTypeBind :: TypeBindBase NoInfo Name -> [(Int, Comment)] -> ([Doc], Int, SrcLoc)
formatTypeBind (TypeBind name l params te _ docComment _) coms' = do
  let (expDoc, consumed) = constructTypeExpDoc te coms' []
  (prettyDocComment docComment :
    text "type" <> ppr l <+> pprName name
    <+> spread (map ppr params)
    <+> equals
    : map (indent 2) expDoc, consumed, srclocOf te)

formatExpBase :: (Eq vn, IsName vn, Annot f) => ExpBase f vn -> [(Int, Comment)] -> [Doc] -> SrcLoc -> ([Doc], Int, SrcLoc)
formatExpBase exp zipComs tmpDoc lastSrcLoc = do
  let (docBeforeExp, consumed) = commentsBefore zipComs (srclocOf exp) lastSrcLoc tmpDoc False
  case exp of 
    Parens e srcloc -> formatExpBase e (drop consumed zipComs) tmpDoc srcloc
    AppExp e _ -> do
      case e of
        DoLoop _ _ _ _ _ sloc -> do
          let (comsDoc', consumed') = commentsBefore (drop consumed zipComs) sloc lastSrcLoc tmpDoc False
          (docBeforeExp ++  comsDoc' ++ [ppr exp], consumed', srclocOf e)
        LetPat sizes pat e' body srcloc -> do
          let (commentsBeforeBody, consumedBeforeBody) = commentsBefore (drop consumed zipComs) srcloc lastSrcLoc docBeforeExp False
          let expDocWithoutBody = firstpat --[align $ text "let" <+> spread (map ppr sizes) <+> align (ppr pat) <> space] 
                    where 
                      linebreak = case e' of
                        AppExp {} -> True
                        Attr {} -> True
                        ArrayLit {} -> False
                        _ -> hasArrayLit e'
                      firstpat = if linebreak
                              then align (text "let" <+> spread (map ppr sizes) <+> align (ppr pat) <+> equals) : [indent 2 (ppr e')]
                              else [align $ text "let" <+> spread (map ppr sizes) <+> align (ppr pat) <+> equals <+> align (ppr e')]
          let (bodyDoc, consumedAfterBody, srcLocOfLastLine) = letBody body (drop consumedBeforeBody zipComs) (commentsBeforeBody ++ expDocWithoutBody) srcloc
          (commentsBeforeBody ++ bodyDoc, consumedAfterBody, srcLocOfLastLine) -- doesnt affect summation 
        _ -> (docBeforeExp ++ [ppr exp], consumed, srclocOf e)-- doesnt affect summation 
    _ -> (docBeforeExp ++ [ppr exp], consumed, srclocOf exp)-- doesnt affect summation 

letBody :: (Eq vn, IsName vn, Annot f) => ExpBase f vn -> [(Int, Comment)] -> [Doc] -> SrcLoc -> ([Doc], Int, SrcLoc)
letBody body@(AppExp LetPat {} _) zipComs tmpDoc srcLocOfBody = formatExpBase body zipComs tmpDoc srcLocOfBody
letBody body@(AppExp LetFun {} _) zipComs tmpDoc srcLocOfBody = formatExpBase body zipComs tmpDoc srcLocOfBody
letBody body zipComs tmpDoc lastsrcloc = do
    let (commentsBeforeBody, consumed) = commentsBefore zipComs (srclocOf body) lastsrcloc tmpDoc False
    let bodyDoc = [text "in" <+> align (ppr body)]
    (commentsBeforeBody ++ bodyDoc, consumed, srclocOf body) -- doesnt affect

constructTypeExpDoc :: TypeExp Name -> [(Int, Comment)] -> [Doc] -> ([Doc], Int)
constructTypeExpDoc exp coms' expDoc =
  case coms' of 
    [] -> (expDoc ++ [ppr exp], 10000)
    _ ->  if checkComment coms' (srclocOf exp) then do
            let tmpDoc = expDoc ++ [align . text . unpackCommentString $ head $ unzipComs coms']
            constructTypeExpDoc exp (tail coms') tmpDoc
          else (expDoc ++ [ppr exp], fst $ head coms')

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
              -- let coms = prepareComments comments []
              -- let b = head coms `isLocatedBefore` srclocOf (head decs)
              -- print b
              --print $ head decs
              --writeFile file $ show $ head decs
              writeFile ("fmt." ++ file)
                $ trim
                $ PP.prettyCompact
                $ prettyDocComment doc 
                </> formatSource decs (prepareComments comments []) [] (srclocOf $ last decs) -- write fmt to file

              writeFile ("tree." ++ file) $ show $ head decs
              -- case decs of
              --   [] -> pure ()
              --   db : dbs -> case db of
              --     ValDec vbb -> case vbb of
              --       ValBind m_ni na m_te ni tpbs pbs eb m_dc ais sl -> case pbs of
              --         [] -> pure ()
              --         pb : pbs' -> do 
              --           print $ endColOfLoc (locOf pb)
              --           print $ startColOfLoc (locOf pb)
              --     TypeDec tbb -> pure ()
              --     SigDec sbb -> pure ()
              --     ModDec mbb -> pure ()
              --     OpenDec meb sl -> pure ()
              --     LocalDec db' sl -> pure ()
              --     ImportDec str ni sl -> pure ()

              --print $ zip [0..] comments
              --print $ (locOf $ srclocOf $ tail decs) > (unpackTokLoc $ comments!!2)
              --putStrLn $ PP.pretty 80 $ prettyDocComment doc <> prettySource decs comments --write fmt to stdout
    _ -> Nothing