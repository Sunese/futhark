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

genComsDoc :: [(Int, Comment)] -> SrcLoc -> [Doc] -> ([Doc], Int)
genComsDoc comments sloc doc = 
  case comments of 
    [] -> (doc, 10000)
    _ -> 
      if checkComment comments sloc
        then genComsDoc -- insert comment (and newline) and recurse
              (tail comments) 
              sloc 
              (doc ++ [text $ unpackCommentString $ head $ unzipComs comments])
        else (doc, fst $ head comments)

prepareComments :: [L Token] -> [Comment] -> [Comment]
prepareComments lToks coms = 
  case lToks of
    [] -> coms
    _ -> prepareComments (tail lToks) (coms ++ [Comment (unpackComTokString $ head lToks) (unpackTokSrcLoc $ head lToks) OnNextLine])

changeCommentPos :: Comment -> Comment
changeCommentPos (Comment string srcloc position) = 
  if position == OnNextLine
    then Comment string srcloc OnTheSameLine
    else Comment string srcloc position

insertComment :: Comment -> SrcLoc -> [Doc] -> [Doc]
insertComment (Comment string loc position) sloc docs = 
  if startLineOfSrcLoc sloc == startLineOfSrcLoc (srclocOf loc) then
    -- append to doc at last entry of [Doc]
    let doc = last docs
        doc' = doc <+> text string
    in dropEnd1 docs ++ [doc']
  else
    docs ++ [text string]


formatSource :: [UncheckedDec] -> [Comment] -> [Doc] -> SrcLoc -> Doc
formatSource decsQueue coms docs lastSrcLoc =
  case decsQueue of
    [] -> 
      case coms of
        [] -> stack docs -- we are done
        _ -> formatSource decsQueue (tail coms) (insertComment (head coms) lastSrcLoc docs) lastSrcLoc
          --formatSource decs (tail coms) (docs ++ [text . unpackCommentString $ head coms])  --no more decs, but still some coms, consume rest
    _ -> 
      case coms of
        [] -> formatSource (tail decsQueue) coms (docs ++ [ppr (head decsQueue)]) lastSrcLoc -- still some decs, but no coms, ppr rest
        _ -> do
          if head coms `isLocatedBefore` srclocOf (head decsQueue) then
            formatSource decsQueue (tail coms) (docs ++ [text . unpackCommentString $ head coms]) lastSrcLoc
          else do
            let (decDoc, consumed, lastSrcLoc') = formatDec (head decsQueue) (zip [0..] coms)
            formatSource (tail decsQueue) (drop consumed coms) (docs ++ decDoc) lastSrcLoc'

formatDec :: UncheckedDec -> [(Int, Comment)] -> ([Doc], Int, SrcLoc)
formatDec dec zipComs = 
  case dec of
    ValDec dec' -> formatValBind dec' zipComs
    TypeDec dec' -> formatTypeBind dec' zipComs
    SigDec sig -> ([ppr sig], 0, srclocOf sig)
    ModDec sd -> ([ppr sd], 0, srclocOf sd)
    OpenDec x _ -> ([text "open" <+> ppr x], 0, srclocOf x)
    LocalDec dec' _ -> do 
      let (doc', consumed, lastSrcLoc) = formatDec dec' zipComs
      (text "local" <+> head doc' : tail doc', consumed, srclocOf dec')
    ImportDec x _ _ -> ([text "import" <+> dquotes (ppr x)], 0, srclocOf dec)

formatValBind :: ValBindBase NoInfo Name -> [(Int, Comment)] -> ([Doc], Int, SrcLoc)
formatValBind dec zipComs = do
  let (expDoc, consumed, lastSrcLoc) = constructExpDoc (bodyOf dec) zipComs $ srclocOf dec
  (constructDocWithoutBody dec : map (indent 2) expDoc, consumed, lastSrcLoc)

constructDocWithoutBody :: (Annot f, Eq v, IsName v) => ValBindBase f v -> Doc
constructDocWithoutBody (ValBind entry name retdecl rettype tparams args _ docCom attrs _) = 
  mconcat (map ((<> line) . ppr) attrs)
    <> prettyDocComment docCom
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

formatTypeBind :: TypeBindBase NoInfo Name -> [(Int, Comment)] -> ([Doc], Int, SrcLoc)
formatTypeBind (TypeBind name l params te _ docComment _) coms' = do
  let (expDoc, consumed) = constructTypeExpDoc te coms' []
  (prettyDocComment docComment :
    text "type" <> ppr l <+> pprName name
    <+> spread (map ppr params)
    <+> equals
    : map (indent 2) expDoc, consumed, srclocOf te)

constructExpDoc :: ExpBase NoInfo Name -> [(Int, Comment)] -> SrcLoc -> ([Doc], Int, SrcLoc)
constructExpDoc exp coms' lastSrcLoc = do
  let (comsDoc, consumed) = genComsDoc coms' (srclocOf exp) []
  case exp of 
    -- sloc of exp and sloc of each AppExp is the same
    Parens e srcloc -> constructExpDoc e (drop consumed coms') srcloc
    AppExp e _ -> do
      case e of
        DoLoop _ _ _ _ _ sloc -> do
          let (comsDoc', consumed') = genComsDoc (drop consumed coms') sloc []
          (comsDoc ++  comsDoc' ++ [ppr exp], consumed', srclocOf e)
        LetPat _ _ _ body _ -> do
          let (comsDoc', consumed') = genComsDoc (drop consumed coms') (srclocOf body) []
          -- let bodydoc = 
          --   parensIf (p /= -1) $
          --     align $
          --       text "let" <+> spread (map ppr sizes) <+> align (ppr pat)
          --         <+> ( if linebreak
          --                 then equals </> indent 2 (ppr e)
          --                 else equals <+> align (ppr e)
          --             )
          --         </> letBody body
          --   where
          --     linebreak = case e of
          --       AppExp {} -> True
          --       Attr {} -> True
          --       ArrayLit {} -> False
          --       _ -> hasArrayLit e
          (comsDoc ++ comsDoc' ++ [ppr exp], consumed', srclocOf e)
        _ -> (comsDoc ++ [ppr exp], consumed, srclocOf e)
    _ -> (comsDoc ++ [ppr exp], consumed, srclocOf exp)

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
                $ PP.pretty 80 
                $ prettyDocComment doc 
                </> formatSource decs (prepareComments comments []) [] (srclocOf $ last decs) -- write fmt to file

              --writeFile ("tree" ++ file) $ show $ head decs
              --print $ zip [0..] comments
              --print $ (locOf $ srclocOf $ tail decs) > (unpackTokLoc $ comments!!2)
              --putStrLn $ PP.pretty 80 $ prettyDocComment doc <> prettySource decs comments --write fmt to stdout
    _ -> Nothing