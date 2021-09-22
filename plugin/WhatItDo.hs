{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BangPatterns, FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module WhatItDo ( plugin, traceDo ) where

-- base
-- ghc
import qualified GHC.ThToHs as GHC
import qualified BasicTypes as GHC
import qualified GHC
import qualified GhcPlugins as GHC
import qualified GHC.Hs.Expr as Expr
import qualified IfaceEnv as GHC
import qualified RnExpr as GHC
import qualified TcEnv as GHC
import qualified TcEvidence as GHC
import qualified TcExpr as GHC
import qualified TcHsSyn as GHC
import qualified TcRnMonad as GHC
import qualified TcSMonad as GHC ( runTcS )
import qualified TcSimplify as GHC
import qualified TcType as GHC
import qualified TyCoRep as GHC
import Finder as GHC
import Outputable ( ($$) )
import TcInteract as GHC
import Constraint as GHC
import TcOrigin

import Data.Generics ( GenericM, Data, gmapM, ext0 )

-- template-haskell
import Language.Haskell.TH as TH

-- what-it-do
import qualified OurConstraint

-- For the instrumentation payload, for now:
import qualified Data.Text as T
import Control.Monad.Reader

import Debug.Trace
import GHC.Hs.Binds
import Var (EvVar)
import Data.Typeable
import qualified TcErrors as GHC
import Data.Maybe
import TcMType (newFlexiTyVar, newWanted)
import Type (liftedTypeKind)

import Control.Concurrent (myThreadId)
import System.IO.Unsafe
 

-- TODO ...
--  - improve panic and warning messages

-- When true, report unsolvable constraint errors rather than just not instrumenting
developing :: Bool
developing = False

data Env = Env { loc :: Maybe GHC.SrcSpan, evidence :: [EvVar]  }

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.typeCheckResultAction = \_cliOptions -> pluginImpl
    , GHC.pluginRecompile = GHC.purePlugin
    -- TODO this is 'pure' right?
    }


pluginImpl :: GHC.ModSummary -> GHC.TcGblEnv -> GHC.TcM GHC.TcGblEnv
pluginImpl _modSummary tcGblEnv = do
  hscEnv <-
    GHC.getTopEnv

  GHC.Found _ assertExplainerModule <-
    liftIO
      ( GHC.findImportedModule
          hscEnv
          ( GHC.mkModuleName "WhatItDo" )
          Nothing
      )

    -- TODO noTrace for disabling
  _traceDoName <-
    GHC.lookupId
      =<< GHC.lookupOrig assertExplainerModule ( GHC.mkVarOcc "traceDo" )

  -- Constructor 'MonadReader Text' Type:
  monadReader <- getClassOrTypeName "Control.Monad.Reader.Class" "MonadReader"
  text <- getClassOrTypeName "Data.Text.Internal" "Text"

  let monadReaderText = (monadReader, [text]) :: OurConstraint

  -- The logic for rewriting expressions:
  let instrumentDoBlock :: ((Expr.HsExpr GHC.GhcTc) -> ReaderT Env GHC.TcM ( Expr.HsExpr GHC.GhcTc ))
                        -> Expr.HsExpr GHC.GhcTc -> ReaderT Env GHC.TcM ( Expr.HsExpr GHC.GhcTc )
      instrumentDoBlock k v = do
        case v of
          (Expr.HsDo ty Expr.DoExpr _ ) -> do
            res <- k v
            Env loc vars <- ask
            lift $ ourRewriteDoExpr vars ty monadReaderText (fromMaybe GHC.noSrcSpan loc) res
          -- Add evidence from local wrappers
          (Expr.HsWrap _ wrap _) -> do
            let ev_vars = gatherEvVars wrap []
            local (\e -> e { evidence = evidence e ++ ev_vars}) (k v)

          _x -> k v

      -- Find each binding, then instrument each internal do_block based on
      -- the local evidence introduced on that binding.
      addLocalEvidence :: ((HsBindLR GHC.GhcTc GHC.GhcTc -> ReaderT Env GHC.TcM b)
                  -> HsBindLR GHC.GhcTc GHC.GhcTc -> ReaderT Env GHC.TcM b)
      addLocalEvidence k v =
        case v of
          ab@GHC.AbsBinds {} -> do
              local (\e -> e { evidence = evidence e ++ (GHC.abs_ev_vars ab)}) (k v)
          x -> k x

      locations ::  (Typeable e) => (GHC.GenLocated GHC.SrcSpan e
                   -> ReaderT Env GHC.TcM (GHC.GenLocated GHC.SrcSpan e))
                  -> GHC.GenLocated GHC.SrcSpan e -> ReaderT Env GHC.TcM (GHC.GenLocated GHC.SrcSpan e)
      locations k l@(GHC.L cur_loc _) = local (\e -> e { loc = Just cur_loc}) (k l)

      traversal :: Typeable a0 => ((a0 -> ReaderT Env GHC.TcM a0)
                  -> a0 -> ReaderT Env GHC.TcM a0)
      traversal = (mkM2 addLocalEvidence `extM2` instrumentDoBlock)
                                         `extM2` (locations @(GHC.HsExpr GHC.GhcTc))

  tcg_binds <- flip runReaderT (Env Nothing []) $
      -- TODO everywhereButM where not noTrace ?
    everywhereM' (traversal) (GHC.tcg_binds tcGblEnv)

  return tcGblEnv { GHC.tcg_binds = tcg_binds }

gatherEvVars :: GHC.HsWrapper -> [EvVar] -> [EvVar]
gatherEvVars (GHC.WpEvLam ev) acc = GHC.pprTrace "tylam" (GHC.ppr (GHC.idType ev)) ev : acc
gatherEvVars (GHC.WpCompose w1 w2) acc = gatherEvVars w1 (gatherEvVars w2 acc)
gatherEvVars _ acc = acc

newtype M2 m a = M2 { unM2 :: (a -> m a) -> a -> m a }

extM2 :: (Typeable a, Typeable b) => ((a -> m a) -> a -> m a) -> ((b -> m b) -> b -> m b) -> (a -> m a) -> a -> m a
extM2 def ext = unM2 ((M2 def) `ext0` (M2 ext))

mkM2 :: (Typeable a, Typeable b, Functor f) => ((b -> f b) -> b -> f b) -> (a -> f a) -> a -> f a
mkM2 ext = extM2 (\k a -> k a) ext

everywhereM' :: forall m. Monad m => (forall a . Data a => (a -> m a) -> a -> m a) -> GenericM m
everywhereM' f = go
  where
    go :: forall a . Data a => a -> m a
    go x = do
      res <- f (gmapM go) x
      return res


-- TODO: we might instead want a noTrace function to disable...
traceDo :: m a -> m a
traceDo =
  id

-- TODO can we get this to work on ApplicativeDo blocks?
-- TODO we also want to get our threadId and include that here
-- TODO also make this a bracket pattern if IO-ish? Maybe only inject when MonadBaseControl IO m ?
--      else we'd need a heuristic in sidecar to handle missing END markers
traceInstrumentationBasic :: (Monad m)=> String -> m a -> m a
traceInstrumentationBasic locString = \m -> do
    -- NOTE: we need !() <-... here to force a data dependency for lazy monads, e.g. ((->) a)
    !() <- noop_traceM
    a <- m  -- TODO we might also attach the END to WHNF of `a` itself, but we'd have
            --      no way to be sure it would be evaluated.
            --        This would be interesting as an additional standalone
            --        trace log we can link back to the START/END span
    !() <- noop_traceM
    return a
{-# INLINE traceInstrumentationBasic #-}

-- Proof of concept: we can inject different code based on results of type checking:
traceInstrumentationWithContext :: (MonadReader T.Text m)=> String -> m a -> m a
traceInstrumentationWithContext locString = \m -> do
    t <- ask
    !() <- noop_traceM
    a <- m
    !() <- noop_traceM
    return a
{-# INLINE traceInstrumentationWithContext #-}

noop_traceM :: Applicative f => f ()
{-# NOINLINE noop_traceM #-}
noop_traceM = unsafePerformIO $ do
    !_ <- return ()
    return $ pure ()

ourRewriteDoExpr
  :: [GHC.EvVar]
  -> GHC.Type
  -> OurConstraint
  -- ^ @MonadReader Text@ for now.
  -> GHC.SrcSpan
  -> Expr.HsExpr GHC.GhcTc
  -> GHC.TcM (Expr.HsExpr GHC.GhcTc)
ourRewriteDoExpr local_ev_vars doExprT monadReaderText loc doExpr = GHC.setSrcSpan loc $ GHC.checkNoErrs $ do
  -- Make the givens from the local environment.
  lcl_env <- GHC.getLclEnv
  let ct = CtLoc DefaultOrigin lcl_env Nothing initialSubGoalDepth
      givens = mkGivens ct local_ev_vars
  canAsk <- hasInstance givens monadReaderText doExprT
  let
    ppWhere =
      GHC.renderWithStyle
        GHC.unsafeGlobalDynFlags
        ( GHC.ppr loc )
        ( GHC.defaultUserStyle GHC.unsafeGlobalDynFlags )

  Right traceExprPs <-
      -- TODO is GHC.Generated right here? or FromSource?
    fmap ( GHC.convertToHsExpr GHC.Generated loc )
      $ liftIO
      $ TH.runQ
      $ if canAsk
           then [| (traceInstrumentationWithContext ppWhere) |]
           else [| (traceInstrumentationBasic ppWhere) |]

  ( traceExprRn, _ ) <-
    GHC.rnLExpr traceExprPs

  ( traceExprTc, wanteds ) <-
    GHC.captureTopConstraints
      ( GHC.tcMonoExpr
          traceExprRn
          ( GHC.Check ( GHC.mkFunTy GHC.VisArg doExprT doExprT ) )
      )

  -- Solve wanted constraints and build a wrapper.
  (wc, ev_binds_map) <- GHC.runTcS (GHC.solveSimpleGivens givens >> GHC.solveWanteds wanteds )

  when developing $ GHC.reportAllUnsolved wc

  if not (isEmptyWC wc)
    then do
      GHC.addWarn GHC.NoReason (GHC.text "Unable to transform" GHC.<> GHC.colon GHC.<+> GHC.text (if canAsk then "MR" else "basic") $$ GHC.ppr wc)
      return doExpr
    else do
      let evBinds = GHC.EvBinds . GHC.evBindMapBinds $ ev_binds_map


      emptyZonkEnv <- GHC.emptyZonkEnv
      ( _, zonkedEvBinds ) <-
        GHC.zonkTcEvBinds emptyZonkEnv evBinds

      let
        wrapper =
          GHC.mkWpLet zonkedEvBinds
      --------------- TODO can some of this chunk of work above be done just once, in the caller?

      -- Apply the wrapper to our type checked syntax and fully saturate the
      -- diagnostic function with the necessary arguments.
      newBody <-
        GHC.zonkTopLExpr
          ( GHC.mkHsApp
             ( GHC.mkLHsWrap wrapper traceExprTc )
              (GHC.noLoc doExpr)
          )

      return (GHC.unLoc newBody)



-- | (Type class name , Optional type arguments to type class), e.g.
-- representing @MonadReader Text@
type OurConstraint = (GHC.Name , [GHC.Name])

-- TODO maybe this needs just a list of Names passed in
-- | Given the 'GHC.Name' of a class C (see 'getClassName'), and a typed
-- expression, ensure that it has an instance of C.
hasInstance :: [Ct] -> OurConstraint -> GHC.Type -> GHC.TcM Bool
hasInstance givens (className,classArgs) t = do
  classTyCon  <-
    GHC.tcLookupTyCon className
  -- NOTE: internally tcMetaTy is just: tcLookupTyCon + mkTyConApp []
  classArgTys <- mapM GHC.tcMetaTy classArgs

  dictName <-
    GHC.newName ( GHC.mkDictOcc ( GHC.mkVarOcc "magic" ) )


  m <- newFlexiTyVar (GHC.mkVisFunTy liftedTypeKind liftedTypeKind)
  a <- newFlexiTyVar liftedTypeKind

  let eq_pred = GHC.mkPrimEqPredRole GHC.Nominal (GHC.mkAppTy (GHC.mkTyVarTy m) (GHC.mkTyVarTy a)) t

  let
    dict_ty =
      GHC.mkTyConApp classTyCon (classArgTys ++ [ GHC.mkTyVarTy m ])

    dict_var =
      GHC.mkVanillaGlobal dictName dict_ty

  eq_wanted <- newWanted DefaultOrigin Nothing eq_pred

  ebs <-
    OurConstraint.getDictionaryBindings givens [mkNonCanonical eq_wanted] dict_var

  return (isJust ebs)

-- | Lookup a type class or type by module and name:
getClassOrTypeName :: String -> String -> GHC.TcM GHC.Name
getClassOrTypeName moduleString classString = do
  hsc_env <- GHC.getTopEnv
  lookup_res <- liftIO $ findImportedModule hsc_env (GHC.mkModuleName moduleString) Nothing
  case lookup_res of
    Found _ modul -> GHC.lookupOrig modul (GHC.mkOccName GHC.tcClsName classString)
    NoPackage uid -> GHC.pprPanic "NoPackage" (GHC.ppr uid)
    -- TODO better error; we get this when user doesn't add mtl as dependency (for now):
    NotFound a b c d e _f -> GHC.pprPanic "NotFound" (GHC.ppr a $$ GHC.ppr b $$ GHC.ppr c $$ GHC.ppr d $$ GHC.ppr e)
    _ -> GHC.pprPanic "not found" GHC.empty
