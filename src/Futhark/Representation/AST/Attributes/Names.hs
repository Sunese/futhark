{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
-- | Facilities for determining which names are used in some syntactic
-- construct.  The most important interface is the 'FreeIn' class and
-- its instances, but for reasons related to the Haskell type system,
-- some constructs have specialised functions.
module Futhark.Representation.AST.Attributes.Names
       (
         -- * Class
           FreeIn (..)
         -- * Specialised Functions
         , freeInBody
         , freeInExp
         , freeInLambda
         , freeInExtLambda
         -- * Bound Names
         , progNames
       )
       where

import Control.Monad.Writer
import qualified Data.HashSet as HS

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Traversals
import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.RetType

freeWalker :: (FreeIn (Lore.Exp lore),
               FreeIn (Lore.Body lore),
               FreeIn (Lore.FParam lore),
               FreeIn (Lore.LetBound lore)) =>
              Walker lore (Writer Names)
freeWalker = identityWalker {
               walkOnSubExp = subExpFree
             , walkOnBody = bodyFree
             , walkOnBinding = bindingFree
             , walkOnLambda = lambdaFree
             , walkOnExtLambda = extLambdaFree
             , walkOnVName = tell . HS.singleton
             , walkOnCertificates = tell . HS.fromList
             }
  where subExpFree = tell . freeIn

        lambdaFree = tell . freeInLambda

        extLambdaFree = tell . freeInExtLambda

        typeFree = tell . freeIn

        binding bound = censor (`HS.difference` bound)

        bodyFree (Body lore [] ses) = do
          tell $ freeIn lore
          mapM_ subExpFree ses
        bodyFree (Body lore (Let pat annot e:bnds) res) = do
          expFree e
          tell $ freeIn annot
          binding (HS.fromList $ patternNames pat) $ do
            mapM_ typeFree $ patternTypes pat
            bodyFree $ Body lore bnds res

        bindingFree (Let pat annot e) = do
          tell $ freeIn annot
          binding (HS.fromList $ patternNames pat) $ do
            tell $ freeInPattern pat
            expFree e

        expFree (LoopOp (DoLoop _ merge (ForLoop i boundexp) loopbody)) = do
          let (mergepat, mergeexps) = unzip merge
          mapM_ subExpFree mergeexps
          subExpFree boundexp
          binding (i `HS.insert` HS.fromList (map fparamName mergepat)) $ do
            mapM_ (tell . freeIn) mergepat
            bodyFree loopbody

        expFree (LoopOp (DoLoop _ merge (WhileLoop cond) loopbody)) = do
          let (mergepat, mergeexps) = unzip merge
          mapM_ subExpFree mergeexps
          tell $ freeIn cond
          binding (HS.fromList (map fparamName mergepat)) $ do
            mapM_ (tell . freeIn) mergepat
            bodyFree loopbody

        expFree e = walkExpM freeWalker e

-- | Return the set of variable names that are free in the given body.
freeInBody :: (FreeIn (Lore.Exp lore),
               FreeIn (Lore.Body lore),
               FreeIn (Lore.FParam lore),
               FreeIn (Lore.LetBound lore)) =>
              Body lore -> Names
freeInBody = execWriter . walkOnBody freeWalker

-- | Return the set of variable names that are free in the given
-- expression.
freeInExp :: (FreeIn (Lore.Exp lore),
              FreeIn (Lore.Body lore),
              FreeIn (Lore.FParam lore),
              FreeIn (Lore.LetBound lore)) =>
             Exp lore -> Names
freeInExp = execWriter . walkExpM freeWalker

-- | Return the set of variable names that are free in the given
-- lambda, including shape annotations in the parameters.
freeInLambda :: (FreeIn (Lore.Exp lore),
                 FreeIn (Lore.Body lore),
                 FreeIn (Lore.FParam lore),
                 FreeIn (Lore.LetBound lore)) =>
                Lambda lore -> Names
freeInLambda (Lambda params body rettype) =
  inRet <> inParams <> inBody
  where inRet = mconcat $ map freeIn rettype
        inParams = mconcat $ map freeInParam params
        freeInParam = freeIn . identType
        inBody = HS.filter (`notElem` paramnames) $ freeInBody body
        paramnames = map identName params

-- | Return the set of identifiers that are free in the given
-- existential lambda, including shape annotations in the parameters.
freeInExtLambda :: (FreeIn (Lore.Exp lore),
                    FreeIn (Lore.Body lore),
                    FreeIn (Lore.FParam lore),
                    FreeIn (Lore.LetBound lore)) =>
                   ExtLambda lore -> Names
freeInExtLambda (ExtLambda params body rettype) =
  inRet <> inParams <> inBody
  where inRet = mconcat $ map freeIn rettype
        inParams = mconcat $ map freeInParam params
        freeInParam = freeIn . identType
        inBody = HS.filter (`notElem` paramnames) $ freeInBody body
        paramnames = map identName params

freeInPattern :: FreeIn (Lore.LetBound lore) => Pattern lore -> Names
freeInPattern (Pattern context values) =
  mconcat $ map freeIn $ context ++ values

-- | A class indicating that we can obtain free variable information
-- from values of this type.
class FreeIn a where
  freeIn :: a -> Names

instance FreeIn () where
  freeIn () = mempty

instance (FreeIn a, FreeIn b) => FreeIn (a,b) where
  freeIn (a,b) = freeIn a <> freeIn b

instance (FreeIn a, FreeIn b, FreeIn c) => FreeIn (a,b,c) where
  freeIn (a,b,c) = freeIn a <> freeIn b <> freeIn c

instance FreeIn a => FreeIn [a] where
  freeIn = mconcat . map freeIn

instance FreeIn Bool where
  freeIn _ = mempty

instance FreeIn a => FreeIn (Maybe a) where
  freeIn = maybe mempty freeIn

instance FreeIn VName where
  freeIn = HS.singleton

instance FreeIn Ident where
  freeIn ident = identName ident `HS.insert` freeIn (identType ident)

instance FreeIn SubExp where
  freeIn (Var v) = freeIn v
  freeIn (Constant {}) = mempty

instance FreeIn Shape where
  freeIn = mconcat . map freeIn . shapeDims

instance FreeIn ExtShape where
  freeIn = mconcat . map freeInExtDimSize . extShapeDims
    where freeInExtDimSize (Free se) = freeIn se
          freeInExtDimSize (Ext _)   = mempty

instance (ArrayShape shape, FreeIn shape) => FreeIn (TypeBase shape) where
  freeIn (Array _ shape _) = freeIn shape
  freeIn (Mem size)        = freeIn size
  freeIn (Basic _)         = mempty

instance FreeIn attr => FreeIn (FParamT attr) where
  freeIn (FParam ident attr) =
    freeIn ident <> freeIn attr

instance FreeIn attr => FreeIn (PatElemT attr) where
  freeIn (PatElem ident bindage attr) =
    freeIn ident <> freeIn bindage <> freeIn attr

instance FreeIn Bindage where
  freeIn BindVar = mempty
  freeIn (BindInPlace cs src is) =
    freeIn cs <> freeIn src <> freeIn is

instance FreeIn ExtRetType where
  freeIn = mconcat . map freeIn . retTypeValues

instance FreeIn LoopForm where
  freeIn (ForLoop _ bound) = freeIn bound
  freeIn (WhileLoop cond) = freeIn cond

-- | Return the set of all variable names bound in a program.
progNames :: Prog lore -> Names
progNames = execWriter . mapM funNames . progFunctions
  where names = identityWalker {
                  walkOnBinding = bindingNames
                , walkOnBody = bodyNames
                , walkOnLambda = lambdaNames
                , walkOnExtLambda = extLambdaNames
                }

        one = tell . HS.singleton
        funNames fundec = do
          mapM_ (one . fparamName) $ funDecParams fundec
          bodyNames $ funDecBody fundec

        bodyNames = mapM_ bindingNames . bodyBindings

        bindingNames (Let pat _ e) =
          mapM_ one (patternNames pat) >> expNames e

        expNames (LoopOp (DoLoop _ pat form loopbody)) = do
          mapM_ (one . fparamName . fst) pat
          case form of ForLoop i _ ->
                         one i
                       WhileLoop _ ->
                         return ()
          bodyNames loopbody

        expNames e = walkExpM names e

        lambdaNames (Lambda params body _) =
          mapM_ (one . identName) params >> bodyNames body

        extLambdaNames (ExtLambda params body _) =
          mapM_ (one . identName) params >> bodyNames body
