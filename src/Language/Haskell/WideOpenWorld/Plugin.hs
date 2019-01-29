{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections         #-}

module Language.Haskell.WideOpenWorld.Plugin where

import Bag
import Control.Arrow (second)
import Var
import Generics.SYB hiding (Fixity (..))
import IOEnv
import Class
import Control.Monad
import Convert (convertToHsDecls)
import CoreSyn
import CoreUtils
import Data.Foldable
import Data.List (uncons)
import Data.Maybe
import DsBinds
import DsMonad
import GHC.WhyArentYouExported
import HsDecls
import HscTypes
import Language.Haskell.TH hiding (Type, ppr)
import Language.Haskell.TH.Syntax hiding (Type)
import Language.Haskell.WideOpenWorld.Test
import MkCore
import Module (mkModuleName)
import OccName (mkTcOcc)
import OrdList
import Outputable hiding ((<>))
import Plugins (Plugin (..), defaultPlugin)
import RnSource
import SrcLoc (noSrcSpan)
import TcEvidence
import TcInstDcls
import TcPluginM
import TcRnDriver
import TcRnTypes
import TyCon
import Type
import Unsafe.Coerce

newtype IOEnv' env a = IOEnv' (env -> IO a)


plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const (Just jdiPlugin) }

jdiPlugin :: TcPlugin
jdiPlugin =
  TcPlugin { tcPluginInit  = pure ()
           , tcPluginSolve = solveJDI
           , tcPluginStop  = const (return ())
           }

lookupJDITyCon :: TcPluginM Class
lookupJDITyCon = do
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


solveJDI :: () -- ^ JDI's TyCon
         -> [Ct]  -- ^ [G]iven constraints
         -> [Ct]  -- ^ [D]erived constraints
         -> [Ct]  -- ^ [W]anted constraints
         -> TcPluginM TcPluginResult
solveJDI jdiCls _ _ wanteds = do
  (env, _) <- getEnvs
  if (not $ null wanteds)
     then do
        pprTraceM "what" (ppr wanteds)
        (bs, e) <- unsafeTcPluginTcM $ cool env stuff
        for_ bs setEvBind
        pure $ TcPluginOk (fmap (e,) wanteds) []
     else pure $ TcPluginOk [] []

cool :: TcGblEnv -> [Dec] -> TcM ([EvBind], EvTerm)
cool env z = unsafeCoerce $ IOEnv' $ \env -> runIOEnv (env { env_gbl = clearTcGblEnv $ env_gbl env } ) $ do
  let Right m = convertToHsDecls noSrcSpan z
  l <- tcRnSrcDecls m
  x <- initDsTc $ dsTopLHsBinds $ tcg_binds l
  let binds = drop 1 $ fromOL x
      dict = fst $ head binds
      evBinds = fmap (uncurry mkGivenEvBind . second EvExpr)
              $ binds
      ev = mkLetRec binds $ Var dict
      newName = mkLocalVar (idDetails dict)
  pure $ (evBinds, EvExpr $ Var dict )
  -- pure $ EvExpr $ everywhere (mkT $ changeExport $ varUnique dict) ev
    where
      changeExport uniq z
        | varUnique z == uniq = setIdNotExported z
        | otherwise = z


stuff :: [Dec]
stuff =
  [InstanceD Nothing []
    (AppT (ConT ''Semigroup)
      (ConT ''Int))
        [FunD '(<>) [Clause [] (NormalB (VarE 'ok)) []]]]

