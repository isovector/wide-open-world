{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Haskell.WideOpenWorld.Plugin where

-- external
import Data.Maybe
import Control.Monad
import Data.Foldable

-- GHC API
import Module (mkModuleName)
import OccName (mkTcOcc)
import Plugins (Plugin (..), defaultPlugin)
import TcEvidence
import TcPluginM
import TcRnTypes
import Class
import CoreUtils
import MkCore
import TyCon
import Type
import CoreSyn
import Outputable
import           SrcLoc (noSrcSpan)
import Language.Haskell.TH hiding (Type, ppr)
import RnSource
import           Convert (convertToHsDecls)
import           Language.Haskell.TH.Syntax hiding (Type)
import TcInstDcls
import HsDecls
import TcRnDriver
import DsBinds
import HscTypes
import DsMonad


plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const (Just jdiPlugin) }

jdiPlugin :: TcPlugin
jdiPlugin =
  TcPlugin { tcPluginInit  = lookupJDITyCon
           , tcPluginSolve = solveJDI
           , tcPluginStop  = const (return ())
           }

lookupJDITyCon :: TcPluginM Class
lookupJDITyCon = do
    env <- getTopEnv
    unsafeTcPluginTcM $ cool env stuff
    Found _ md   <- findImportedModule jdiModule Nothing
    jdiTcNm <- lookupOrig md (mkTcOcc "JustDoIt")
    tcLookupClass jdiTcNm
  where
    jdiModule  = mkModuleName "GHC.JustDoIt"

wrap :: Class -> CoreExpr -> EvTerm
wrap cls = EvExpr . appDc
  where
    tyCon = classTyCon cls
    dc = tyConSingleDataCon tyCon
    appDc x = mkCoreConApps dc [Type (exprType x), x]

findClassConstraint :: Class -> Ct -> Maybe (Ct, Type)
findClassConstraint cls ct = do
    (cls', [t]) <- getClassPredTys_maybe (ctPred ct)
    guard (cls' == cls)
    return (ct, t)

solveJDI :: Class -- ^ JDI's TyCon
         -> [Ct]  -- ^ [G]iven constraints
         -> [Ct]  -- ^ [D]erived constraints
         -> [Ct]  -- ^ [W]anted constraints
         -> TcPluginM TcPluginResult
solveJDI jdiCls _ _ wanteds = pure $ TcPluginOk [] []
    -- return $! case result of
    --     Left x       -> TcPluginContradiction [x]
    --     Right solved -> TcPluginOk solved []
  -- where
    -- our_wanteds = mapMaybe (findClassConstraint jdiCls) wanteds
    -- result = partitionMaybe (fmap (wrap jdiCls) . solve) our_wanteds


cool :: HscEnv -> [Dec] -> TcM ()
cool env z = do
  let Right m = convertToHsDecls noSrcSpan z
  (k, _) <- findSplice m
  (_, l) <- rnTopSrcDecls k
  (gbl, lcl) <- tcTopSrcDecls l
  -- tcTopSrcDecls <-- need to zonk before dsTopLHsBinds
  x <- initDsTc $ dsTopLHsBinds $ tcg_binds gbl

  -- (_, x, y) <- tcInstDecls1 $ (group_instds =<<) $ hs_tyclds l
  -- undefined_
  pprPanic "help" $ ppr $ length $ toList x


stuff :: [Dec]
stuff =
  [InstanceD Nothing []
    (AppT (ConT ''Num)
      (AppT ListT (VarT $ Name (OccName "a") (NameU 0))))
        [FunD 'fromInteger [Clause [WildP] (NormalB (ListE [])) []]]]


-- dsTopLHsBinds
