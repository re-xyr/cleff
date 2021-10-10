module Effect.Internal.TH where

import           Control.Monad                (join)
import           Data.Char                    (toLower)
import           Data.Foldable                (foldl')
import qualified Data.Map.Strict              as Map
import           Effect.Internal.Monad
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.PprLib
import           Prelude                      hiding ((<>))

-- | Generate sending functions for each of the effects in the given effect type.
makeEffect :: Name -> Q [Dec]
makeEffect = makeSmartCons True

-- | Generate sending functions for each of the effects in the given effect type, but without type signatures. You can
-- add your signature /after/ placing this splice.
makeEffect_ :: Name -> Q [Dec]
makeEffect_ = makeSmartCons False

-- | Generate sending functions for each of the effects in the given type. Whether to generate type signatures is
-- decided by the first argument.
makeSmartCons :: Bool -> Name -> Q [Dec]
makeSmartCons makeSig effName = do
  info <- reifyDatatype effName
  join <$> traverse (makeCon makeSig) (constructorName <$> datatypeCons info)

-- | Generate a sending function for a particular constructor.
makeCon :: Bool -> Name -> Q [Dec]
makeCon makeSig name = do
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

    substMnd monadVar effVar parTy = applySubstitution (Map.singleton monadVar $ ConT ''Eff `AppT` effVar) parTy
