{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BangPatterns, FlexibleContexts #-}

module WhatItDo ( plugin, traceDo ) where

-- base
import Data.Traversable ( for )
import Control.Monad.IO.Class ( liftIO )
import Data.Foldable ( toList )
import Debug.Trace ( traceM )

-- ghc
import qualified GHC.ThToHs as GHC
import qualified BasicTypes as GHC
import qualified CoreUtils
import qualified Desugar as GHC
import qualified Finder as GHC
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
import qualified GHC.Paths

-- syb
import Data.Generics ( everywhereM, mkM )

-- template-haskell
import Language.Haskell.TH as TH

-- what-it-do
import qualified OurConstraint

-- For the instrumentation payload, for now:
import qualified Data.Text as T
import Control.Monad.Reader 

import Debug.Trace

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
  traceDoName <-
    GHC.lookupId
      =<< GHC.lookupOrig assertExplainerModule ( GHC.mkVarOcc "traceDo" )

  -- Constructor 'MonadReader Text' Type:
  monadReader <- getClassOrTypeName "Control.Monad.Reader" "MonadReader"
  text <- getClassOrTypeName "Data.Text" "Text"
  let monadReaderText = (monadReader, [text]) :: OurConstraint

  -- The logic for rewriting expressions:
  let instrumentDoBlock :: Expr.LHsExpr GHC.GhcTc -> GHC.TcM ( Expr.LHsExpr GHC.GhcTc )
      instrumentDoBlock =
        \case
          doExpr@(GHC.L loc ( Expr.HsDo _ Expr.DoExpr _ )) ->
            ourRewriteDoExpr monadReaderText loc doExpr

          x -> return x

  tcg_binds <-
      -- TODO everywhereButM where not noTrace ?
    mkM instrumentDoBlock `everywhereM` GHC.tcg_binds tcGblEnv

  return tcGblEnv { GHC.tcg_binds = tcg_binds }


-- TODO: we might instead want a noTrace function to disable...
traceDo :: m a -> m a
traceDo =
  id



-- TODO we also want to get our threadId and include that here
-- TODO also make this a bracket pattern if IO-ish? Maybe only inject when MonadBaseControl IO m ?
--      else we'd need a heuristic in sidecar to handle missing END markers
traceInstrumentationBasic :: (Monad m)=> String -> m a -> m a
traceInstrumentationBasic locString = \m -> do
    traceM $ "XXXXX START " ++ locString
    !a <- m -- TODO we might not wish to evaluate to WHNF here, or provide options
            -- TODO we might also attach the END to WHNF of `a` itself, but we'd have
            --      no way to be sure it would be evaluated.
            --        This would be interesting as an additional standalone
            --        trace log we can link back to the START/END span
    traceM $ "XXXXX END   " ++ locString
    return a
{-# INLINE traceInstrumentationBasic #-}

-- Proof of concept: we can inject different code based on results of type checking:
traceInstrumentationWithContext :: (MonadReader T.Text m)=> String -> m a -> m a
traceInstrumentationWithContext locString = \m -> do
    t <- ask
    traceM $ "XXXXX START -- context: " ++ (T.unpack t) ++ " -- " ++ locString
    !a <- m
    traceM $ "XXXXX END   " ++ locString
    return a
{-# INLINE traceInstrumentationWithContext #-}


ourRewriteDoExpr
  :: OurConstraint
  -- ^ @MonadReader Text@ for now.
  -> GHC.SrcSpan
  -> Expr.LHsExpr GHC.GhcTc
  -> GHC.TcM (Expr.LHsExpr GHC.GhcTc)
ourRewriteDoExpr monadReaderText loc doExpr = do
  Just doExprT <- typeOfExpr doExpr
  -- let ( _m, a ) = GHC.splitAppTy doExprT  -- NOTE, for if we want to inspect return type
  canAsk <- hasInstance monadReaderText doExprT
  let
    ppWhere =
      GHC.renderWithStyle
        GHC.unsafeGlobalDynFlags
        ( GHC.ppr loc )
        ( GHC.defaultUserStyle GHC.unsafeGlobalDynFlags )

  Right traceExprPs <-
      -- TODO is GHC.Generated right here? or FromSource?
    fmap ( GHC.convertToHsExpr GHC.Generated GHC.noSrcSpan )
      $ liftIO
      $ TH.runQ
      $ if canAsk
           then [| (traceInstrumentationWithContext ppWhere) |]
           else [| (traceInstrumentationBasic ppWhere) |]

  ( traceExprRn, _ ) <-
    GHC.rnLExpr traceExprPs

  ( traceExprTc, wanteds ) <-
    GHC.captureConstraints
      ( GHC.tcMonoExpr
          traceExprRn
          ( GHC.Check ( GHC.mkFunTy GHC.VisArg doExprT doExprT ) )
      )

  -- Solve wanted constraints and build a wrapper.
  evBinds <-
    GHC.EvBinds . GHC.evBindMapBinds . snd
      <$> GHC.runTcS ( GHC.solveWanteds wanteds )

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
          doExpr
      )

  return newBody


typeOfExpr :: Expr.LHsExpr GHC.GhcTc -> GHC.TcM ( Maybe GHC.Type )
typeOfExpr e = do
  hs_env  <- GHC.getTopEnv
  ( _, mbe ) <- liftIO ( GHC.deSugarExpr hs_env e )
  return ( CoreUtils.exprType <$> mbe )


-- | (Type class name , Optional type arguments to type class), e.g.
-- representing @MonadReader Text@
type OurConstraint = (GHC.Name , [GHC.Name]) 

-- TODO maybe this needs just a list of Names passed in
-- | Given the 'GHC.Name' of a class C (see 'getClassName'), and a typed
-- expression, ensure that it has an instance of C.
hasInstance :: OurConstraint -> GHC.Type -> GHC.TcM Bool
hasInstance (className,classArgs) t = do
  classTyCon  <-
    GHC.tcLookupTyCon className
  -- NOTE: internally tcMetaTy is just: tcLookupTyCon + mkTyConApp []
  classArgTys <- mapM GHC.tcMetaTy classArgs

  dictName <-
    GHC.newName ( GHC.mkDictOcc ( GHC.mkVarOcc "magic" ) )

  let
    dict_ty =
      GHC.mkTyConApp classTyCon (classArgTys ++ [ t ])

    dict_var =
      GHC.mkVanillaGlobal dictName dict_ty

  GHC.EvBinds evBinds <-
    OurConstraint.getDictionaryBindings dict_var

  return ( not ( null ( toList evBinds ) ) )

-- | Lookup a type class or type by module and name:
getClassOrTypeName :: String -> String -> GHC.TcM GHC.Name
getClassOrTypeName moduleString classString = do
  -- TODO is runGhc expensive to do here?
  -- TODO is our use of ghc-paths right here?
  modul <- liftIO $ GHC.runGhc Nothing $   -- ERROR: Missing file: /home/me/.ghcup/ghc/8.10.2/lib/ghc-8.10.2/lib/settings
  -- modul <- liftIO $ GHC.runGhc (Just GHC.Paths.libdir) $
      GHC.lookupModule (GHC.mkModuleName moduleString) Nothing
  GHC.lookupOrig modul (GHC.mkOccName GHC.tcClsName classString)
