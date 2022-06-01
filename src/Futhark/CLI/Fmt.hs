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
import Futhark.Util.Pretty
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
  commentSrcLoc :: Loc,
  commentPosition :: CommentPosition
}

bodyOf :: ValBindBase f vn -> ExpBase f vn
bodyOf (ValBind _ _ _ _ _ _ body _ _ _) = body

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

formatSource :: [UncheckedDec] -> [Comment] -> [Doc] -> Doc
formatSource decs coms docs =
  case decs of
    [] -> 
      case coms of
        [] -> stack docs -- we are done
        _ -> formatSource decs (tail coms) (docs ++ [text . unpackCommentString $ head coms])  --no more decs, but still some coms, consume rest
    _ -> 
      case coms of
        [] -> formatSource (tail decs) coms (docs ++ [ppr (head decs)]) -- still some decs, but no coms, ppr rest
        _ -> do
          if head coms `isLocatedBefore` srclocOf (head decs) then
            formatSource decs (tail coms) (docs ++ [text . unpackCommentString $ head coms])
          else do
            let (decDoc, consumed) = formatDec (head decs) (zip [0..] coms)
            formatSource (tail decs) (drop consumed coms) (docs ++ decDoc)
  where
    formatDec :: UncheckedDec -> [(Int, Comment)] -> ([Doc], Int)
    formatDec dec zipComs = 
      case dec of
        ValDec dec' -> formatValBind dec' zipComs
        TypeDec dec' -> formatTypeBind dec' zipComs
        SigDec sig -> ([ppr sig], 0)
        ModDec sd -> ([ppr sd], 0)
        OpenDec x _ -> ([text "open" <+> ppr x], 0)
        LocalDec dec' _ -> do 
          let (doc', consumed) = formatDec dec' zipComs
          (text "local" <+> head doc' : tail doc', consumed)
        ImportDec x _ _ -> ([text "import" <+> dquotes (ppr x)], 0)

    formatValBind :: ValBindBase NoInfo Name -> [(Int, Comment)] -> ([Doc], Int)
    formatValBind dec zipComs = do
      let (expDoc, consumed) = constructExpDoc (bodyOf dec) zipComs
      (constructDocWithoutBody dec : map (indent 2) expDoc, consumed)

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

    formatTypeBind :: TypeBindBase NoInfo Name -> [(Int, Comment)] -> ([Doc], Int)
    formatTypeBind (TypeBind name l params te _ docComment _) coms' = do
      let (expDoc, consumed) = constructTypeExpDoc te coms' mempty
      (prettyDocComment docComment :
        text "type" <> ppr l <+> pprName name
        <+> spread (map ppr params)
        <+> equals
        : map (indent 2) expDoc, consumed)

    constructExpDoc :: ExpBase NoInfo Name -> [(Int, Comment)] -> ([Doc], Int)
    constructExpDoc exp coms' = do
      let (comsDoc, consumed) = genComsDoc coms' (srclocOf exp) mempty
      case exp of 
        -- sloc of exp and sloc of each AppExp is the same
        Parens e _ -> constructExpDoc e (drop consumed coms')
        AppExp e _ -> do
          case e of
            DoLoop _ _ _ _ _ sloc -> do
              let (comsDoc', consumed') = genComsDoc (drop consumed coms') sloc []
              (comsDoc ++  comsDoc' ++ [ppr exp], consumed')
            LetPat _ _ _ body _ -> do
              let (comsDoc', consumed') = genComsDoc (drop consumed coms') (srclocOf body) []
              (comsDoc ++ comsDoc' ++ [ppr exp], consumed')
            _ -> (comsDoc ++ [ppr exp], consumed)
        _ -> (comsDoc ++ [ppr exp], consumed)

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
              </> formatSource decs (prepareComments comments []) [] -- write fmt to file
              --print $ zip [0..] comments
              --print $ (locOf $ srclocOf $ tail decs) > (unpackTokLoc $ comments!!2)
              --putStrLn $ PP.pretty 80 $ prettyDocComment doc <> prettySource decs comments --write fmt to stdout
    _ -> Nothing