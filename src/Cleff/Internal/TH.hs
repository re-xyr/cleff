-- | This module contains Template Haskell functions for generating definitions of functions that send effect
-- operations. You mostly won't want to import this module directly; The "Cleff" module reexports the main
-- functionalities of this module.
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
module Cleff.Internal.TH where

import           Cleff.Internal.Effect
import           Cleff.Internal.Monad
import           Control.Monad                (join)
import           Data.Char                    (toLower)
import           Data.Foldable                (foldl')
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (maybeToList)
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.PprLib
import           Prelude                      hiding ((<>))

-- | For a datatype @T@ representing an effect, @'makeEffect' T@ generates functions defintions for performing the
-- operations (/i.e./ constructors) of @T@ via 'send'. The naming rule is changing the first uppercase letter in the
-- constructor name to lowercase or removing the @:@ symbol in the case of operator constructors. Also, this function
-- will preserve any fixity declarations defined on the constructors.
--
-- Because of the limitations of Template Haskell, all constructors of @T@ should be /polymorphic in the monad type/,
-- if they are to be used by 'makeEffect'. For example, this is not OK:
--
-- @
-- data Limited :: 'Effect' where
--   Noop :: Limited ('Eff' es) ()
-- @
--
-- because the monad type @'Eff' es@ is not a fully polymorphic type variable.
--
-- This function is also "weaker" than @polysemy@'s @makeSem@, because this function cannot properly handle some
-- cases involving complex higher order effects. Those cases are rare, though. See the tests for more details.
makeEffect :: Name -> Q [Dec]
makeEffect = makeSmartCons True

-- | Like 'makeEffect', but doesn't generate type signatures. This is useful when you want to attach Haddock
-- documentation to the function signature, /e.g./:
--
-- @
-- data Identity :: 'Effect' where
--   Noop :: Identity m ()
-- 'makeEffect_' ''Identity
--
-- -- | Perform nothing at all.
-- noop :: Identity ':>' es => 'Eff' es ()
-- @
--
-- Be careful that the function signatures must be added /after/ the 'makeEffect_' call.
makeEffect_ :: Name -> Q [Dec]
makeEffect_ = makeSmartCons False

-- | This is the function underlying 'makeEffect' and 'makeEffect_'. You can switch between the behavior of two by
-- changing the 'Bool' parameter to 'True' (generating signatures) or 'False' (not generating signatures).
makeSmartCons :: Bool -> Name -> Q [Dec]
makeSmartCons makeSig effName = do
  info <- reifyDatatype effName
  join <$> traverse (makeCon makeSig) (constructorName <$> reverse (datatypeCons info))

-- | Make a single function definition of a certain effect operation.
makeCon :: Bool -> Name -> Q [Dec]
makeCon makeSig name = do
  fixity <- reifyFixity name
  typ <- reify name >>= \case
    DataConI _ typ _ -> pure typ
    _ -> fail $ show
      $ text "'" <> ppr name <> text "' is not a constructor"

  effVar <- VarT <$> newName "es"

  let actionCtx = extractCtx typ
  (actionPar, (effTy, monadVar, resTy)) <- extractPar typ

  let fnName = mkName $ toSmartConName $ nameBase name
  fnArgs <- traverse (const $ newName "x") actionPar

  let
    fnBody = VarE 'send `AppE` foldl' (\f -> AppE f . VarE) (ConE name) fnArgs
    fnSig = ForallT [] (UInfixT effTy ''(:>) effVar : actionCtx)
      (makeTyp actionPar effVar effTy monadVar resTy)

  pure $
    maybeToList ((`InfixD` name) <$> fixity) ++
    [ SigD fnName fnSig | makeSig ] ++
    [ FunD fnName [Clause (VarP <$> fnArgs) (NormalB fnBody) []] ]

  where
    toSmartConName (':' : xs) = xs
    toSmartConName (x : xs)   = toLower x : xs
    toSmartConName _          = error "empty identifier name"

    extractCtx (ForallT _ ctx t) = ctx ++ extractCtx t
    extractCtx _                 = []

    extractPar (ForallT _ _ t) = extractPar t
    extractPar (SigT t _) = extractPar t
    extractPar (ParensT t) = extractPar t
    extractPar (ArrowT `AppT` a `AppT` t) = do
      (args, ret) <- extractPar t
      pure (a : args, ret)
#if MIN_VERSION_template_haskell(2,17,0)
    extractPar (MulArrowT `AppT` _ `AppT` a `AppT` t) = do
      (args, ret) <- extractPar t
      pure (a : args, ret)
#endif

    extractPar (effTy `AppT` VarT monadVar `AppT` resTy) = pure ([], (effTy, monadVar, resTy))
    extractPar ty@(_ `AppT` m `AppT` _) = fail $ show
      $ text "The effect monad argument '" <> ppr m
      <> text "' in the effect '" <> ppr ty <> text "' is not a type variable"
    extractPar t = fail $ show
      $ text "The type '" <> ppr t
      <> text "' does not have the shape of an effect (i.e. has a polymorphic monad type and a result type)"

    makeTyp [] effVar _ _ resTy = ConT ''Eff `AppT` effVar `AppT` resTy
    makeTyp (parTy : pars) effVar effTy monadVar resTy =
      ArrowT `AppT` substMnd monadVar effVar parTy `AppT` makeTyp pars effVar effTy monadVar resTy

    substMnd monadVar effVar = applySubstitution (Map.singleton monadVar $ ConT ''Eff `AppT` effVar)
