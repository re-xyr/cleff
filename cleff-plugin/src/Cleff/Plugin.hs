{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use list literal" #-}
module Cleff.Plugin (plugin) where

import           Control.Monad           (filterM)
import           Data.Function           (on)
import           Data.IORef              (IORef, modifyIORef, newIORef, readIORef)
import           Data.Maybe              (catMaybes, isNothing, mapMaybe)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Traversable        (for)
import           GHC.TcPluginM.Extra     (lookupModule, lookupName)

#if __GLASGOW_HASKELL__ >= 900
import           GHC.Core.Class          (Class)
import           GHC.Core.InstEnv        (lookupInstEnv)
import           GHC.Core.Unify          (tcUnifyTy)
import           GHC.Plugins             (Outputable (ppr), Plugin (pluginRecompile, tcPlugin), PredType,
                                          Role (Nominal), TCvSubst, Type, defaultPlugin, eqType, fsLit, mkModuleName,
                                          mkTcOcc, nonDetCmpType, purePlugin, showSDocUnsafe, splitAppTys, substTys,
                                          tyConClass_maybe)
import           GHC.Tc.Plugin           (tcLookupClass, tcPluginIO)
import           GHC.Tc.Solver.Monad     (newWantedEq, runTcSDeriveds)
import           GHC.Tc.Types            (TcM, TcPlugin (TcPlugin, tcPluginInit, tcPluginSolve, tcPluginStop),
                                          TcPluginM, TcPluginResult (TcPluginOk), unsafeTcPluginTcM)
import           GHC.Tc.Types.Constraint (Ct (CDictCan, CNonCanonical), CtEvidence (CtWanted), CtLoc, ctPred)
import           GHC.Tc.Utils.Env        (tcGetInstEnvs)
import           GHC.Tc.Utils.TcType     (tcSplitTyConApp)

#else
import           Class                   (Class)
#if __GLASGOW_HASKELL__ >= 810
import           Constraint              (Ct (CDictCan, CNonCanonical), CtEvidence (CtWanted), CtLoc, ctPred)
#endif
import           GhcPlugins              (Outputable (ppr), Plugin (pluginRecompile, tcPlugin), PredType,
                                          Role (Nominal), TCvSubst, Type, defaultPlugin, eqType, fsLit, mkModuleName,
                                          mkTcOcc, nonDetCmpType, purePlugin, showSDocUnsafe, splitAppTys, substTys,
                                          tyConClass_maybe)
import           InstEnv                 (lookupInstEnv)
import           TcEnv                   (tcGetInstEnvs)
import           TcPluginM               (tcLookupClass, tcPluginIO)
import           TcRnTypes
import           TcSMonad                (newWantedEq, runTcSDeriveds)
import           TcType                  (tcSplitTyConApp)
import           Unify                   (tcUnifyTy)
#endif

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = const (Just fakedep)
  , pluginRecompile = purePlugin
  }

fakedep :: TcPlugin
fakedep = TcPlugin
  { tcPluginInit = initFakedep
  , tcPluginSolve = solveFakedepForAllElemClasses
  , tcPluginStop = const $ pure ()
  }

type SolnPair = (OrdType, OrdType)
type VisitedSet = Set SolnPair

liftTc :: TcM a -> TcPluginM a
liftTc = unsafeTcPluginTcM

liftIo :: IO a -> TcPluginM a
liftIo = tcPluginIO

forMaybe :: Applicative f => [a] -> (a -> f (Maybe b)) -> f [b]
forMaybe xs f = catMaybes <$> traverse f xs

elemClassesNames :: [(String, String, String)]
elemClassesNames =
#ifdef CLEFF_PLUGIN_cleff
  ("cleff", "Cleff.Internal.Rec", "Elem") :
#endif
#ifdef CLEFF_PLUGIN_effectful
  ("effectful", "Effectful.Internal.Effect", ":>") :
#endif
  []

initFakedep :: TcPluginM ([Class], IORef VisitedSet)
initFakedep = do
  classes <- for elemClassesNames \(packageName, elemModuleName, elemClassName) -> do
    recMod <- lookupModule (mkModuleName elemModuleName) $ fsLit packageName
    nm <- lookupName recMod $ mkTcOcc elemClassName
    tcLookupClass nm
  visited <- liftIo $ newIORef Set.empty
  pure (classes, visited)

data FakedepGiven = FakedepGiven
  { givenEffHead :: Type
  , givenEff     :: Type
  , givenEs      :: Type
  }

instance Show FakedepGiven where
  show (FakedepGiven _ e es) = "(Elem " <> showSDocUnsafe (ppr e) <> " " <> showSDocUnsafe (ppr es) <> ")"

data FakedepWanted = FakedepWanted FakedepGiven CtLoc

instance Show FakedepWanted where
  show (FakedepWanted given _) = show given

newtype OrdType = OrdType { unOrdType :: Type }

instance Eq OrdType where
  (==) = eqType `on` unOrdType

instance Ord OrdType where
  compare = nonDetCmpType `on` unOrdType

solveFakedepForAllElemClasses :: ([Class], IORef VisitedSet) -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveFakedepForAllElemClasses (elemClasses, visitedRef) givens _ wanteds = do
  solns <- concat <$> for elemClasses \elemCls -> solveFakedep (elemCls, visitedRef) givens wanteds
  pure $ TcPluginOk [] solns

solveFakedep :: (Class, IORef VisitedSet) -> [Ct] -> [Ct] -> TcPluginM [Ct]
solveFakedep _ _ [] = pure []
solveFakedep (elemCls, visitedRef) allGivens allWanteds = do
  -- We're given two lists of constraints here:
  -- - 'allGivens' are constraints already in our context,
  -- - 'allWanteds' are constraints that need to be solved.
  -- In the following notes, the words "give/given" and "want/wanted" all refer to this specific technical concept:
  -- given constraints are those that we can use, and wanted constraints are those that we need to solve.
  let
    -- The only type of constraint we're interested in solving are 'Elem e es' constraints. Therefore, we extract these
    -- constraints out of the 'allGivens' and 'allWanted's.
    givens = mapMaybe relevantGiven allGivens
    wanteds = mapMaybe relevantWanted allWanteds
    -- We store a set of the types of all given constraints, which will be useful later.
    allGivenTypes = Set.fromList $ OrdType . ctPred <$> allGivens
    -- We also store a list of wanted constraints that are /not/ 'Elem e es' for later use.
    extraWanteds = ctPred <$> filter irrelevant allWanteds

  -- traceM $ "Givens: " <> show (showSDocUnsafe . ppr <$> allGivens)
  -- traceM $ "Wanteds: " <> show (showSDocUnsafe . ppr <$> allWanteds)

  -- For each 'Elem e es' we /want/ to solve (the "goal"), we need to eventually correspond it to another unique
  -- /given/ 'Elem e es' that will make the program typecheck (the "solution").
  solns <- forMaybe wanteds \goal -> solve goal givens allGivenTypes extraWanteds

  -- Now we need to tell GHC the solutions. The way we do this is to generate a new equality constraint, like
  -- 'Elem (State e) es ~ Elem (State Int) es', so that GHC's constraint solver will know that 'e' must be 'Int'.
  eqns <- for solns \(FakedepWanted (FakedepGiven _ goalEff _) loc, FakedepGiven _ solnEff _)  -> do
    (eqn, _) <- liftTc $ runTcSDeriveds $ newWantedEq loc Nominal goalEff solnEff
    pure (CNonCanonical eqn, (OrdType goalEff, OrdType solnEff))

  -- For any solution we've generated, we need to be careful not to generate it again, or we might end up generating
  -- infinitely many solutions. So, we record any already generated solution in a set.
  visitedSolnPairs <- liftIo $ readIORef visitedRef
  let solnEqns = fst <$> flip filter eqns \(_, pair) -> Set.notMember pair visitedSolnPairs
  liftIo $ modifyIORef visitedRef (Set.union $ Set.fromList $ snd <$> eqns)

  -- traceM $ "Emitting: " <> showSDocUnsafe (ppr solnEqns)
  pure solnEqns -- Finally we tell GHC the solutions.

  where

    -- Determine if there is a unique solution to a goal from a set of candidates.
    solve :: FakedepWanted -> [FakedepGiven] -> Set OrdType -> [PredType] -> TcPluginM (Maybe (FakedepWanted, FakedepGiven))
    solve goal@(FakedepWanted (FakedepGiven _ _ goalEs) _) givens allGivenTypes extraWanteds = do
      -- Apart from 'Elem' constraints in the context, the effects already hardwired into the effect stack type,
      -- like those in 'A : B : C : es', also need to be considered. So here we extract that for them to be considered
      -- simultaneously with regular 'Elem' constraints.
      let cands = extractExtraGivens goalEs goalEs <> givens
      -- The first criteria is that the candidate constraint must /unify/ with the goal. This means that the type
      -- variables in the goal can be instantiated in a way so that the goal becomes equal to the candidate.
      -- For example, the candidates 'Elem (State Int) es' and 'Elem (State String) es' both unify with the goal
      -- 'Elem (State s) es'.
      let unifiableCands = mapMaybe (unifiableWith goal) cands
      case unifiableCands of
        -- If there's already only one unique solution, commit to it; in the worst case where it doesn't actually match,
        -- we get a cleaner error message like "Unable to match (State String) to (State Int)" instead of a type
        -- ambiguity error.
        [(soln, _)] -> pure $ Just (goal, soln)
        _ -> do
          -- Otherwise, the second criteria comes in: the candidate must satisfy all other constraints we /want/ to solve.
          -- For example, when we want to solve '(Elem (State a) es, Num a)`, the candidate 'Elem (State Int) es' will do
          -- the job, because it satisfied 'Num a'; however 'Elem (State String) es' will be excluded.
          satisfiableCands <- filterM (satisfiable allGivenTypes extraWanteds) unifiableCands
          -- traceM $ "Viable candidates for " <> show goal <> ": " <> show (fst <$> satisfiableCands)
          -- Finally, if there is a unique candidate remaining, we use it as the solution; otherwise we don't solve anything.
          case satisfiableCands of
            [(soln, _)] -> pure $ Just (goal, soln)
            _           -> pure Nothing

    -- Extract the heads of a type like 'A : B : C : es' into 'FakedepGiven's.
    extractExtraGivens :: Type -> Type -> [FakedepGiven]
    extractExtraGivens fullEs es = case splitAppTys es of
      (_colon, [_kind, e, es']) ->
        let (dtHead, _tyArgs) = splitAppTys e
        in FakedepGiven dtHead e fullEs : extractExtraGivens fullEs es'
      _ -> []

    -- Determine whether a given constraint is of form 'Elem e es'.
    relevantGiven :: Ct -> Maybe FakedepGiven
    relevantGiven (CDictCan _ cls [_kind, eff, es] _)
      | cls == elemCls = Just $ FakedepGiven (fst $ splitAppTys eff) eff es
    relevantGiven (CDictCan _ cls [eff, es] _)
      | cls == elemCls = Just $ FakedepGiven (fst $ splitAppTys eff) eff es
    relevantGiven _ = Nothing

    -- Determine whether a wanted constraint is of form 'Elem e es'.
    relevantWanted :: Ct -> Maybe FakedepWanted
    relevantWanted (CDictCan (CtWanted _ _ _ loc) cls [_kind, eff, es] _) -- cleff's (:>) is polymorphic
      | cls == elemCls = Just $ FakedepWanted (FakedepGiven (fst $ splitAppTys eff) eff es) loc
    relevantWanted (CDictCan (CtWanted _ _ _ loc) cls [eff, es] _) -- effectful's (:>) is monomorphic
      | cls == elemCls = Just $ FakedepWanted (FakedepGiven (fst $ splitAppTys eff) eff es) loc
    relevantWanted _ = Nothing

    -- Determine whether a constraint is /not/ of form 'Elem e es'.
    irrelevant :: Ct -> Bool
    irrelevant = isNothing . relevantGiven

    -- Given a wanted constraint and a given constraint, unify them and give back a substitution that can be applied
    -- to the wanted to make it equal to the given.
    unifiableWith :: FakedepWanted -> FakedepGiven -> Maybe (FakedepGiven, TCvSubst)
    unifiableWith (FakedepWanted goal _) cand =
      -- First, the 'es' type must be equal, and the datatype head of the effect must be equal too.
      if givenEs goal `eqType` givenEs cand && givenEffHead goal `eqType` givenEffHead cand
        then (cand, ) <$> tcUnifyTy (givenEff goal) (givenEff cand) -- Then the effect type must unify.
        else Nothing

    -- Check whether a candidate can satisfy all tthe wanted constraints.
    satisfiable :: Set OrdType -> [PredType] -> (FakedepGiven, TCvSubst) -> TcPluginM Bool
    satisfiable given wanted (_, subst) = do
      instEnv <- liftTc tcGetInstEnvs -- Get the global instances environment.
      let wantedInst = substTys subst wanted
      pure $ flip all wantedInst \want ->
        if Set.member (OrdType want) given then True -- Can we find this constraint in our local context?
        else let (con, args) = tcSplitTyConApp want
        in case tyConClass_maybe con of -- If not, lookup the global environment.
          Nothing  -> False
          Just cls -> let (res, _, _) = lookupInstEnv False instEnv cls args in not $ null res
