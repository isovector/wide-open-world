{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections         #-}

module Language.Haskell.WideOpenWorld.Plugin where

-- external
import Data.Maybe
import Control.Monad
import Data.Foldable

import GHC.WhyArentYouExported
import Data.List (uncons)

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
import Outputable hiding ((<>))
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
import OrdList


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
  if (not $ null wanteds)
     then do
        pprTraceM "what" (ppr wanteds)
        e <- unsafeTcPluginTcM $ cool stuff
        pure $ TcPluginOk (fmap (e,) wanteds) []
     else pure $ TcPluginOk [] []


cool :: [Dec] -> TcM EvTerm
cool z = do
  let Right m = convertToHsDecls noSrcSpan z
  l <- tcRnSrcDecls m
  x <- initDsTc $ dsTopLHsBinds $ tcg_binds l
  let Just (dict, binds) = uncons $ drop 2 $ fromOL x
      ev = mkLetRec binds $ snd dict
  pure $ EvExpr ev


stuff :: [Dec]
stuff =
  [InstanceD Nothing []
    (AppT (ConT ''Semigroup)
      (ConT ''Int))
        [FunD '(<>) [Clause [] (NormalB (VarE '(+))) []]]]

