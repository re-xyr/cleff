{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- This module contains Template Haskell functions for generating definitions of functions that send effect
-- operations. You mostly won't want to import this module directly; The "Cleff" module reexports the main
-- functionalities of this module.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal.TH (makeEffect, makeEffect_) where

import           Cleff.Internal.Monad
import           Control.Monad                (join)
import           Data.Char                    (toLower)
import           Data.Foldable                (foldl')
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (maybeToList)
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype (ConstructorInfo (constructorName), DatatypeInfo (datatypeCons),
                                               TypeSubstitution (applySubstitution), reifyDatatype)
import           Language.Haskell.TH.PprLib   (text, (<>))
import           Prelude                      hiding ((<>))

-- | For a datatype @T@ representing an effect, @'makeEffect' T@ generates functions defintions for performing the
-- operations of @T@ via 'send'. The naming rule is changing the first uppercase letter in the constructor name to
-- lowercase or removing the @:@ symbol in the case of operator constructors. Also, this function will preserve any
-- fixity declarations defined on the constructors.
--
-- This function is also "weaker" than @polysemy@'s @makeSem@, because this function cannot properly handle some
-- cases involving ambiguous types. Those cases are rare, though. See the @ThSpec@ test spec for more details.
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
makeSmartCons shouldMakeSig effName = do
  info <- reifyDatatype effName
  join <$> traverse (makeCon shouldMakeSig) (constructorName <$> reverse (datatypeCons info))

-- | Make a single function definition of a certain effect operation.
makeCon :: Bool -> Name -> Q [Dec]
makeCon shouldMakeSig name = do
  fixity <- reifyFixity name
  ctorTy <- reify name >>= \case
    DataConI _ ty _ -> pure ty
    _               -> fail $ show $ text "'" <> ppr name <> text "' is not a constructor"

  operationCtx' <- extractCtx ctorTy
  (operationParams', (effTy, effMonad, resTy')) <- extractParams ctorTy

  (esVar, maybeMndVar) <- case effMonad of
    Right m -> do
      fresh <- VarT <$> newName "es"
      pure (fresh, Just m)
    Left v -> pure (VarT v, Nothing)

  let operationCtx = substMnd maybeMndVar esVar <$> operationCtx'
  let operationParams = substMnd maybeMndVar esVar <$> operationParams'
  let resTy = substMnd maybeMndVar esVar resTy'

  let fnName = mkName $ toSmartConName $ nameBase name
  fnArgs <- traverse (const $ newName "x") operationParams

  let
    fnBody = VarE 'send `AppE` foldl' (\f -> AppE f . VarE) (ConE name) fnArgs
    fnSig = ForallT [] (UInfixT effTy ''(:>) esVar : operationCtx)
      (makeTyp operationParams esVar effTy resTy)

  pure $
    maybeToList ((`InfixD` name) <$> fixity) ++
    [ SigD fnName fnSig | shouldMakeSig ] ++
    [ FunD fnName [Clause (VarP <$> fnArgs) (NormalB fnBody) []] ]

  where
    -- Uncapitalize the first letter / remove the ':' in operator constructors
    toSmartConName :: String -> String
    toSmartConName (':' : xs) = xs
    toSmartConName (x : xs)   = toLower x : xs
    toSmartConName _          = error "Cleff.makeEffect: Empty constructor name. Please report this as a bug."

    -- Extract constraints for the constructor (the type is normalized so we don't need to extract recursively)
    extractCtx :: Type -> Q Cxt
    extractCtx (ForallT _ ctx _) = pure ctx
    extractCtx ty = fail $ show $ text "The constructor with type'" <> ppr ty <> text "' does not construct an effect"

    -- Extract (parameter types, (effect type, Eff es / m variable, return type))
    extractParams :: Type -> Q ([Type], (Type, Either Name Name, Type))
    extractParams (ForallT _ _ t) = extractParams t
    extractParams (SigT t _) = extractParams t
    extractParams (ParensT t) = extractParams t
    extractParams (ArrowT `AppT` a `AppT` t) = do
      (args, ret) <- extractParams t
      pure (a : args, ret)
#if MIN_VERSION_template_haskell(2,17,0)
    extractParams (MulArrowT `AppT` _ `AppT` a `AppT` t) = do
      (args, ret) <- extractParams t
      pure (a : args, ret)
#endif
    extractParams (effTy `AppT` VarT mndVar `AppT` resTy) = pure ([], (effTy, Right mndVar, resTy))
    extractParams (effTy `AppT` (ConT eff `AppT` VarT esVar) `AppT` resTy)
      | eff == ''Eff = pure ([], (effTy, Left esVar, resTy))
    extractParams ty@(_ `AppT` m `AppT` _) = fail $ show
      $ text "The effect monad argument '" <> ppr m
      <> text "' in the effect '" <> ppr ty <> text "' is not a type variable nor in shape 'Eff es'"
    extractParams t = fail $ show
      $ text "The type '" <> ppr t
      <> text "' does not have the shape of an effect (i.e. has a polymorphic monad type and a result type)"

    -- Make the type of the smart constructor from params, effect row variable, effect type and result type
    -- Example: a -> m b -> c -> MyEffect m d ==> a -> Eff es b -> c -> Eff es d
    makeTyp :: [Type] -> Type -> Type -> Type -> Type
    makeTyp [] esVar _ resTy = ConT ''Eff `AppT` esVar `AppT` resTy
    makeTyp (parTy : pars) esVar effTy resTy =
      ArrowT `AppT` parTy `AppT` makeTyp pars esVar effTy resTy

    -- Substitute in 'Eff es' for the 'm' variable
    substMnd :: Maybe Name -> Type -> Type -> Type
    substMnd Nothing _           = id
    substMnd (Just mndVar) esVar = applySubstitution (Map.singleton mndVar $ ConT ''Eff `AppT` esVar)
