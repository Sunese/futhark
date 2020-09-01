{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A representation where all bindings are annotated with aliasing
-- information.
module Futhark.IR.Aliases
  ( -- * The Lore definition
    Aliases,
    AliasDec (..),
    VarAliases,
    ConsumedInExp,
    BodyAliasing,
    module Futhark.IR.Prop.Aliases,

    -- * Module re-exports
    module Futhark.IR.Prop,
    module Futhark.IR.Traversals,
    module Futhark.IR.Pretty,
    module Futhark.IR.Syntax,

    -- * Adding aliases
    addAliasesToPattern,
    mkAliasedLetStm,
    mkAliasedBody,
    mkPatternAliases,
    mkBodyAliases,

    -- * Removing aliases
    removeProgAliases,
    removeFunDefAliases,
    removeExpAliases,
    removeStmAliases,
    removeLambdaAliases,
    removePatternAliases,
    removeScopeAliases,

    -- * Tracking aliases
    AliasesAndConsumed,
    trackAliases,
    consumedInStms,
  )
where

import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Data.Maybe
import Futhark.Analysis.Rephrase
import Futhark.Binder
import Futhark.IR.Pretty
import Futhark.IR.Prop
import Futhark.IR.Prop.Aliases
import Futhark.IR.Syntax
import Futhark.IR.Traversals
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import qualified Futhark.Util.Pretty as PP
import GHC.Generics
import Language.SexpGrammar as Sexp
import Language.SexpGrammar.Generic

-- | The lore for the basic representation.
data Aliases lore

-- | A wrapper around 'AliasDec to get around the fact that we need an
-- 'Ord' instance, which 'AliasDec does not have.
newtype AliasDec = AliasDec {unAliases :: Names}
  deriving (Show, Generic)

instance SexpIso AliasDec where
  sexpIso = with $ \vname -> sexpIso >>> vname

instance Semigroup AliasDec where
  x <> y = AliasDec $ unAliases x <> unAliases y

instance Monoid AliasDec where
  mempty = AliasDec mempty

instance Eq AliasDec where
  _ == _ = True

instance Ord AliasDec where
  _ `compare` _ = EQ

instance Rename AliasDec where
  rename (AliasDec names) = AliasDec <$> rename names

instance Substitute AliasDec where
  substituteNames substs (AliasDec names) = AliasDec $ substituteNames substs names

instance FreeIn AliasDec where
  freeIn' = const mempty

instance PP.Pretty AliasDec where
  ppr = PP.commasep . map PP.ppr . namesToList . unAliases

-- | The aliases of the let-bound variable.
type VarAliases = AliasDec

-- | Everything consumed in the expression.
type ConsumedInExp = AliasDec

-- | The aliases of what is returned by the t'Body', and what is
-- consumed inside of it.
type BodyAliasing = ([VarAliases], ConsumedInExp)

instance
  (Decorations lore, CanBeAliased (Op lore)) =>
  Decorations (Aliases lore)
  where
  type LetDec (Aliases lore) = (VarAliases, LetDec lore)
  type ExpDec (Aliases lore) = (ConsumedInExp, ExpDec lore)
  type BodyDec (Aliases lore) = (BodyAliasing, BodyDec lore)
  type FParamInfo (Aliases lore) = FParamInfo lore
  type LParamInfo (Aliases lore) = LParamInfo lore
  type RetType (Aliases lore) = RetType lore
  type BranchType (Aliases lore) = BranchType lore
  type Op (Aliases lore) = OpWithAliases (Op lore)

instance AliasesOf (VarAliases, dec) where
  aliasesOf = unAliases . fst

instance FreeDec AliasDec

withoutAliases ::
  (HasScope (Aliases lore) m, Monad m) =>
  ReaderT (Scope lore) m a ->
  m a
withoutAliases m = do
  scope <- asksScope removeScopeAliases
  runReaderT m scope

instance (ASTLore lore, CanBeAliased (Op lore)) => ASTLore (Aliases lore) where
  expTypesFromPattern =
    withoutAliases . expTypesFromPattern . removePatternAliases

instance (ASTLore lore, CanBeAliased (Op lore)) => Aliased (Aliases lore) where
  bodyAliases = map unAliases . fst . fst . bodyDec
  consumedInBody = unAliases . snd . fst . bodyDec

instance
  PrettyAnnot (PatElemT dec) =>
  PrettyAnnot (PatElemT (VarAliases, dec))
  where
  ppAnnot (PatElem name (AliasDec als, dec)) =
    let alias_comment = PP.oneLine <$> aliasComment name als
     in case (alias_comment, ppAnnot (PatElem name dec)) of
          (_, Nothing) ->
            alias_comment
          (Just alias_comment', Just inner_comment) ->
            Just $ alias_comment' PP.</> inner_comment
          (Nothing, Just inner_comment) ->
            Just inner_comment

instance (ASTLore lore, CanBeAliased (Op lore)) => PrettyLore (Aliases lore) where
  ppExpLore (consumed, inner) e =
    maybeComment $
      catMaybes
        [ exp_dec,
          merge_dec,
          ppExpLore inner $ removeExpAliases e
        ]
    where
      merge_dec =
        case e of
          DoLoop _ merge _ body ->
            let mergeParamAliases fparam als
                  | primType (paramType fparam) =
                    Nothing
                  | otherwise =
                    resultAliasComment (paramName fparam) als
             in maybeComment $
                  catMaybes $
                    zipWith mergeParamAliases (map fst merge) $
                      bodyAliases body
          _ -> Nothing

      exp_dec = case namesToList $ unAliases consumed of
        [] -> Nothing
        als ->
          Just $
            PP.oneLine $
              PP.text "-- Consumes " <> PP.commasep (map PP.ppr als)

maybeComment :: [PP.Doc] -> Maybe PP.Doc
maybeComment [] = Nothing
maybeComment cs = Just $ PP.folddoc (PP.</>) cs

aliasComment :: PP.Pretty a => a -> Names -> Maybe PP.Doc
aliasComment name als =
  case namesToList als of
    [] -> Nothing
    als' ->
      Just $
        PP.oneLine $
          PP.text "-- " <> PP.ppr name <> PP.text " aliases "
            <> PP.commasep (map PP.ppr als')

resultAliasComment :: PP.Pretty a => a -> Names -> Maybe PP.Doc
resultAliasComment name als =
  case namesToList als of
    [] -> Nothing
    als' ->
      Just $
        PP.oneLine $
          PP.text "-- Result of " <> PP.ppr name <> PP.text " aliases "
            <> PP.commasep (map PP.ppr als')

removeAliases :: CanBeAliased (Op lore) => Rephraser Identity (Aliases lore) lore
removeAliases =
  Rephraser
    { rephraseExpLore = return . snd,
      rephraseLetBoundLore = return . snd,
      rephraseBodyLore = return . snd,
      rephraseFParamLore = return,
      rephraseLParamLore = return,
      rephraseRetType = return,
      rephraseBranchType = return,
      rephraseOp = return . removeOpAliases
    }

removeScopeAliases :: Scope (Aliases lore) -> Scope lore
removeScopeAliases = M.map unAlias
  where
    unAlias (LetName (_, dec)) = LetName dec
    unAlias (FParamName dec) = FParamName dec
    unAlias (LParamName dec) = LParamName dec
    unAlias (IndexName it) = IndexName it

removeProgAliases ::
  CanBeAliased (Op lore) =>
  Prog (Aliases lore) ->
  Prog lore
removeProgAliases = runIdentity . rephraseProg removeAliases

removeFunDefAliases ::
  CanBeAliased (Op lore) =>
  FunDef (Aliases lore) ->
  FunDef lore
removeFunDefAliases = runIdentity . rephraseFunDef removeAliases

removeExpAliases ::
  CanBeAliased (Op lore) =>
  Exp (Aliases lore) ->
  Exp lore
removeExpAliases = runIdentity . rephraseExp removeAliases

removeStmAliases ::
  CanBeAliased (Op lore) =>
  Stm (Aliases lore) ->
  Stm lore
removeStmAliases = runIdentity . rephraseStm removeAliases

removeLambdaAliases ::
  CanBeAliased (Op lore) =>
  Lambda (Aliases lore) ->
  Lambda lore
removeLambdaAliases = runIdentity . rephraseLambda removeAliases

removePatternAliases ::
  PatternT (AliasDec, a) ->
  PatternT a
removePatternAliases = runIdentity . rephrasePattern (return . snd)

addAliasesToPattern ::
  (ASTLore lore, CanBeAliased (Op lore), Typed dec) =>
  PatternT dec ->
  Exp (Aliases lore) ->
  PatternT (VarAliases, dec)
addAliasesToPattern pat e =
  uncurry Pattern $ mkPatternAliases pat e

mkAliasedBody ::
  (ASTLore lore, CanBeAliased (Op lore)) =>
  BodyDec lore ->
  Stms (Aliases lore) ->
  Result ->
  Body (Aliases lore)
mkAliasedBody innerlore bnds res =
  Body (mkBodyAliases bnds res, innerlore) bnds res

mkPatternAliases ::
  (Aliased lore, Typed dec) =>
  PatternT dec ->
  Exp lore ->
  ( [PatElemT (VarAliases, dec)],
    [PatElemT (VarAliases, dec)]
  )
mkPatternAliases pat e =
  -- Some part of the pattern may be the context.  This does not have
  -- aliases from expAliases, so we use a hack to compute aliases of
  -- the context.
  let als = expAliases e ++ repeat mempty -- In case the pattern has
  -- more elements (this
  -- implies a type error).
      context_als = mkContextAliases pat e
   in ( zipWith annotateBindee (patternContextElements pat) context_als,
        zipWith annotateBindee (patternValueElements pat) als
      )
  where
    annotateBindee bindee names =
      bindee `setPatElemLore` (AliasDec names', patElemDec bindee)
      where
        names' =
          case patElemType bindee of
            Array {} -> names
            Mem _ -> names
            _ -> mempty

mkContextAliases ::
  Aliased lore =>
  PatternT dec ->
  Exp lore ->
  [Names]
mkContextAliases pat (DoLoop ctxmerge valmerge _ body) =
  let ctx = map fst ctxmerge
      init_als = zip mergenames $ map (subExpAliases . snd) $ ctxmerge ++ valmerge
      expand als = als <> mconcat (mapMaybe (`lookup` init_als) (namesToList als))
      merge_als =
        zip mergenames $
          map ((`namesSubtract` mergenames_set) . expand) $
            bodyAliases body
   in if length ctx == length (patternContextElements pat)
        then map (fromMaybe mempty . flip lookup merge_als . paramName) ctx
        else map (const mempty) $ patternContextElements pat
  where
    mergenames = map (paramName . fst) $ ctxmerge ++ valmerge
    mergenames_set = namesFromList mergenames
mkContextAliases pat (If _ tbranch fbranch _) =
  take (length $ patternContextNames pat) $
    zipWith (<>) (bodyAliases tbranch) (bodyAliases fbranch)
mkContextAliases pat _ =
  replicate (length $ patternContextElements pat) mempty

mkBodyAliases ::
  Aliased lore =>
  Stms lore ->
  Result ->
  BodyAliasing
mkBodyAliases bnds res =
  -- We need to remove the names that are bound in bnds from the alias
  -- and consumption sets.  We do this by computing the transitive
  -- closure of the alias map (within bnds), then removing anything
  -- bound in bnds.
  let (aliases, consumed) = mkStmsAliases bnds res
      boundNames =
        foldMap (namesFromList . patternNames . stmPattern) bnds
      aliases' = map (`namesSubtract` boundNames) aliases
      consumed' = consumed `namesSubtract` boundNames
   in (map AliasDec aliases', AliasDec consumed')

mkStmsAliases ::
  Aliased lore =>
  Stms lore ->
  [SubExp] ->
  ([Names], Names)
mkStmsAliases bnds res = delve mempty $ stmsToList bnds
  where
    delve (aliasmap, consumed) [] =
      ( map (aliasClosure aliasmap . subExpAliases) res,
        consumed
      )
    delve (aliasmap, consumed) (bnd : bnds') =
      delve (trackAliases (aliasmap, consumed) bnd) bnds'
    aliasClosure aliasmap names =
      names <> mconcat (map look $ namesToList names)
      where
        look k = M.findWithDefault mempty k aliasmap

-- | Everything consumed in the given statements and result (even
-- transitively).
consumedInStms :: Aliased lore => Stms lore -> Names
consumedInStms = snd . flip mkStmsAliases []

type AliasesAndConsumed =
  ( M.Map VName Names,
    Names
  )

trackAliases ::
  Aliased lore =>
  AliasesAndConsumed ->
  Stm lore ->
  AliasesAndConsumed
trackAliases (aliasmap, consumed) bnd =
  let pat = stmPattern bnd
      als =
        M.fromList $
          zip (patternNames pat) (map addAliasesOfAliases $ patternAliases pat)
      aliasmap' = als <> aliasmap
      consumed' = consumed <> addAliasesOfAliases (consumedInStm bnd)
   in (aliasmap', consumed')
  where
    addAliasesOfAliases names = names <> aliasesOfAliases names
    aliasesOfAliases = mconcat . map look . namesToList
    look k = M.findWithDefault mempty k aliasmap

mkAliasedLetStm ::
  (ASTLore lore, CanBeAliased (Op lore)) =>
  Pattern lore ->
  StmAux (ExpDec lore) ->
  Exp (Aliases lore) ->
  Stm (Aliases lore)
mkAliasedLetStm pat (StmAux cs attrs dec) e =
  Let
    (addAliasesToPattern pat e)
    (StmAux cs attrs (AliasDec $ consumedInExp e, dec))
    e

instance (Bindable lore, CanBeAliased (Op lore)) => Bindable (Aliases lore) where
  mkExpDec pat e =
    let dec = mkExpDec (removePatternAliases pat) $ removeExpAliases e
     in (AliasDec $ consumedInExp e, dec)

  mkExpPat ctx val e =
    addAliasesToPattern (mkExpPat ctx val $ removeExpAliases e) e

  mkLetNames names e = do
    env <- asksScope removeScopeAliases
    flip runReaderT env $ do
      Let pat dec _ <- mkLetNames names $ removeExpAliases e
      return $ mkAliasedLetStm pat dec e

  mkBody bnds res =
    let Body bodylore _ _ = mkBody (fmap removeStmAliases bnds) res
     in mkAliasedBody bodylore bnds res

instance (ASTLore (Aliases lore), Bindable (Aliases lore)) => BinderOps (Aliases lore)
