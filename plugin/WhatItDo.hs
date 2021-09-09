{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BangPatterns #-}

module WhatItDo ( plugin, traceDo ) where

-- base
import Data.Traversable ( for )
import Control.Monad.IO.Class ( liftIO )
import Data.Foldable ( toList )
import Debug.Trace ( traceM )

-- ghc
import qualified Outputable as PP
import qualified GHC.ThToHs as GHC
import qualified BasicTypes as GHC
import qualified CoreUtils
import qualified Desugar as GHC
import qualified Finder as GHC
import qualified GHC
import qualified GhcPlugins as GHC
import qualified GHC.Hs.Expr as Expr
import qualified IfaceEnv as GHC
import qualified PrelNames as GHC
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

-- syb
import Data.Generics ( everywhereM, mkM )

-- template-haskell
import Language.Haskell.TH as TH

-- what-it-do
import qualified OurConstraint

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.typeCheckResultAction = \_cliOptions -> pluginImpl
    , GHC.pluginRecompile = GHC.purePlugin
    -- ^ TODO okay?
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

  tcg_binds <-
      -- TODO everywhereButM where traceDont
    mkM ( ourTraceDo ) `everywhereM` GHC.tcg_binds tcGblEnv
    -- mkM ( whatItDo traceDoName ) `everywhereM` GHC.tcg_binds tcGblEnv

  return tcGblEnv { GHC.tcg_binds = tcg_binds }


-- TODO: we might want a noTrace function...
traceDo :: m a -> m a
traceDo =
  id

    {-
whatItDo
  :: GHC.Id -> Expr.LHsExpr GHC.GhcTc -> GHC.TcM ( Expr.LHsExpr GHC.GhcTc )
whatItDo traceDoName ( GHC.L _ ( Expr.HsApp _ ( GHC.L _ ( Expr.HsWrap _ _ ( Expr.HsVar _ ( GHC.L _ v ) ) ) ) ( GHC.L _ body ) ) ) | v == traceDoName = do
  GHC.noLoc <$> go body

  where

    go :: Expr.HsExpr GHC.GhcTc -> GHC.TcM (Expr.HsExpr GHC.GhcTc)
    go ( Expr.HsDo t Expr.DoExpr ( GHC.L _ stmts ) ) = do
      stmts' <-
        for
          stmts
          ( \orig@( GHC.L loc stmt ) ->
              case stmt of
                Expr.BindStmt bt pat expr e1 e2 -> do
                  traceBind loc bt pat expr e1 e2

                _ ->
                  return orig
          )

      return ( Expr.HsDo t Expr.DoExpr ( GHC.noLoc stmts' ) )

    go ( Expr.HsPar a ( GHC.L b c ) ) =
      Expr.HsPar a <$> ( GHC.L b <$> go c )

    go o@( Expr.HsVar _ ( GHC.L loc _ ) ) =
      o
        <$ GHC.addWarnAt
             GHC.NoReason
             loc
             ( PP.text "debugDo cannot be used on variables" )

whatItDo _ other =
  return other
-}

-- TODO OUR WIP
-- GHC.L (SrcSpan)
ourTraceDo
  :: Expr.LHsExpr GHC.GhcTc -> GHC.TcM ( Expr.LHsExpr GHC.GhcTc )
ourTraceDo =
  -- fmap GHC.noLoc . \case
  \case
    -- TODO sweet HsDo is HsExpr, so we can treat this like 'expr' in traceBind
    -- x@(GHC.L loc ( Expr.HsDo t Expr.DoExpr ( GHC.L _ stmts ) )) ->
    --   x <$ GHC.addWarnAt
    --          GHC.NoReason
    --          loc
    --          ( PP.text "DOOOOOOOOOOO" )
    doExpr@(GHC.L loc ( Expr.HsDo _ Expr.DoExpr _ )) ->
      ourRewriteDoExpr loc doExpr

    x -> return x
  -- where

  --   go :: Expr.HsExpr GHC.GhcTc -> GHC.TcM (Expr.HsExpr GHC.GhcTc)
  --   go ( Expr.HsDo t Expr.DoExpr ( GHC.L _ stmts ) ) = do
  --     stmts' <-
  --       for
  --         stmts
  --         ( \orig@( GHC.L loc stmt ) ->
  --             case stmt of
  --               Expr.BindStmt bt pat expr e1 e2 -> do
  --                 traceBind loc bt pat expr e1 e2

  --               _ ->
  --                 return orig
  --         )

  --     return ( Expr.HsDo t Expr.DoExpr ( GHC.noLoc stmts' ) )

  --   go ( Expr.HsPar a ( GHC.L b c ) ) =
  --     Expr.HsPar a <$> ( GHC.L b <$> go c )

  --   go o@( Expr.HsVar _ ( GHC.L loc _ ) ) =
  --     o
  --       <$ GHC.addWarnAt
  --            GHC.NoReason
  --            loc
  --            ( PP.text "debugDo cannot be used on variables" )


-- TODO we also want to get our threadId and include that here
-- TODO also make this a bracket pattern if IO-ish? Maybe only inject when MonadBaseControl IO m ?
ourTraceSpan :: (Monad m)=> String -> m a -> m a
ourTraceSpan locString = \m -> do
    traceM $ "XXXXX START " ++ locString
    !a <- m -- TODO we might not wish to evaluate to WHNF here, or provide options
    traceM $ "XXXXX END   " ++ locString
    return a
{-# INLINE ourTraceSpan #-}

ourRewriteDoExpr
  :: GHC.SrcSpan
  -> Expr.LHsExpr GHC.GhcTc
  -> GHC.TcM (Expr.LHsExpr GHC.GhcTc)
ourRewriteDoExpr loc doExpr = do
  Just doExprT <- typeOfExpr doExpr
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
      $ [| (ourTraceSpan ppWhere) |]

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
  --------------- TODO can this chunk of work above be done just once higher up?

  -- Apply the wrapper to our type checked syntax and fully saturate the
  -- diagnostic function with the necessary arguments.
  newBody <-
    GHC.zonkTopLExpr
      ( GHC.mkHsApp
          ( GHC.mkLHsWrap wrapper traceExprTc )
          doExpr
      )

  return newBody

    {-
traceBind
  :: GHC.SrcSpan
  -> GHC.Type
  -> GHC.LPat GHC.GhcTc
  -> Expr.LHsExpr GHC.GhcTc
  -> Expr.SyntaxExpr GHC.GhcTc
  -> Expr.SyntaxExpr GHC.GhcTc
  -> GHC.TcM ( Expr.LStmt GHC.GhcTc ( Expr.LHsExpr GHC.GhcTc ) )
traceBind loc t pat expr e1 e2 = do
  Just exprT <-
    typeOfExpr expr

  let
    ( _m, a ) =
      GHC.splitAppTy exprT

  canShow <-
    hasShow a

  if canShow
    then do
      let
        ppWhere =
          GHC.renderWithStyle
            GHC.unsafeGlobalDynFlags
            ( GHC.ppr loc )
            ( GHC.defaultUserStyle GHC.unsafeGlobalDynFlags )

        ppPat =
          GHC.renderWithStyle
            GHC.unsafeGlobalDynFlags
            ( GHC.ppr pat )
            ( GHC.defaultUserStyle GHC.unsafeGlobalDynFlags )

      Right traceExprPs <-
          -- TODO is GHC.Generated right here? or FromSource?
        fmap ( GHC.convertToHsExpr GHC.Generated GHC.noSrcSpan )
          $ liftIO
          $ TH.runQ
          $ [| ( >>= \x ->
                   trace
                     ( "("
                         ++ ppWhere
                         ++ ") "
                         ++ ppPat
                         ++ " = "
                         ++ show x
                     )
                     ( return x )
               )
            |]

      ( traceExprRn, _ ) <-
        GHC.rnLExpr traceExprPs

      ( traceExprTc, wanteds ) <-
        GHC.captureConstraints
          ( GHC.tcMonoExpr
              traceExprRn
              ( GHC.Check ( GHC.mkFunTy GHC.VisArg exprT exprT ) )
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

      -- Apply the wrapper to our type checked syntax and fully saturate the
      -- diagnostic function with the necessary arguments.
      newBody <-
        GHC.zonkTopLExpr
            -- TODO
            -- GHC.mkHsApp
            --   :: Expr.LHsExpr (GHC.GhcPass id)
            --      -> Expr.LHsExpr (GHC.GhcPass id) -> Expr.LHsExpr (GHC.GhcPass id)
          ( GHC.mkHsApp
              ( GHC.mkLHsWrap wrapper traceExprTc )
              expr
          )

      return ( GHC.L loc ( Expr.BindStmt t pat newBody e1 e2 ) )
  else
    return ( GHC.L loc ( Expr.BindStmt t pat expr e1 e2 ) )
-}

typeOfExpr :: Expr.LHsExpr GHC.GhcTc -> GHC.TcM ( Maybe GHC.Type )
typeOfExpr e = do
  hs_env  <-
    GHC.getTopEnv

  ( _, mbe ) <-
    liftIO ( GHC.deSugarExpr hs_env e )

  return ( CoreUtils.exprType <$> mbe )



-- | Given a typed expression, ensure that it has a Show instance.
hasShow :: GHC.Type -> GHC.TcM Bool
hasShow t = do
  showTyCon <-
    GHC.tcLookupTyCon GHC.showClassName

  dictName <-
    GHC.newName ( GHC.mkDictOcc ( GHC.mkVarOcc "magic" ) )

  let
    dict_ty =
      GHC.mkTyConApp showTyCon [ t ]

    dict_var =
      GHC.mkVanillaGlobal dictName dict_ty

  GHC.EvBinds evBinds <-
    OurConstraint.getDictionaryBindings dict_var

  return ( not ( null ( toList evBinds ) ) )

    {-
showClassName :: Name
showClassName   = clsQual gHC_SHOW (fsLit "Show")      showClassKey
clsQual  = mk_known_key_name clsName 
clsName   = TcClsName  
gHC_SHOW        = mkBaseModule (fsLit "GHC.Show")

showClassKey            = mkPreludeClassUnique 17
mkPreludeClassUnique i = mkUnique '2' i

-- (from module Name)
-- | A unique, unambiguous name for something, containing information about where
-- that thing originated.
data Name = Name {
                n_sort :: NameSort,     -- What sort of name it is
                n_occ  :: !OccName,     -- Its occurrence name
                n_uniq :: {-# UNPACK #-} !Unique,
                n_loc  :: !SrcSpan      -- Definition site
            }
-}
