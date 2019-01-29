{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections         #-}

module Language.Haskell.WideOpenWorld.Plugin
  ( plugin
  , stuff
  ) where

import Control.Arrow (second)
import Control.Monad
import Convert (convertToHsDecls)
import CoreSyn
import Data.Foldable
import DsBinds
import DsMonad
import GHC.WhyArentYouExported
import IOEnv
import Language.Haskell.TH hiding (Type, ppr)
import Language.Haskell.TH.Syntax hiding (Type)
import Language.Haskell.WideOpenWorld.Test
import OrdList
import Outputable hiding ((<>))
import Plugins (Plugin (..), defaultPlugin)
import SrcLoc (noSrcSpan)
import TcEvidence
import TcPluginM
import TcRnTypes
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


solveJDI :: () -- ^ JDI's TyCon
         -> [Ct]  -- ^ [G]iven constraints
         -> [Ct]  -- ^ [D]erived constraints
         -> [Ct]  -- ^ [W]anted constraints
         -> TcPluginM TcPluginResult
solveJDI _ _ _ wanteds = do
  if (not $ null wanteds)
     then do
        pprTraceM "what" (ppr wanteds)
        (bs, e) <- unsafeTcPluginTcM $ cool stuff2
        for_ bs setEvBind
        pure $ TcPluginOk (fmap (e,) wanteds) []
     else pure $ TcPluginOk [] []


localIOEnv :: (g -> g) -> IOEnv (Env g l) a -> IOEnv (Env g l) a
localIOEnv f m
  = unsafeCoerce
  . IOEnv'
  $ flip runIOEnv m
  . \env -> env { env_gbl = f $ env_gbl env }


cool :: [Dec] -> TcM ([EvBind], EvTerm)
cool z = localIOEnv clearTcGblEnv $ do
  let Right m = convertToHsDecls noSrcSpan z
  l <- tcRnSrcDecls m
  void $ pprPanic "hi" $ ppr $ tcg_binds l
  x <- initDsTc $ dsTopLHsBinds $ tcg_binds l
  let binds = drop 1 $ fromOL x
      dict = fst $ head binds
      evBinds = fmap (uncurry mkGivenEvBind . second EvExpr)
              $ binds
  pure $ (evBinds, EvExpr $ Var dict )


stuff :: [Dec]
stuff =
  [InstanceD Nothing []
    (AppT (ConT ''Semigroup)
      (ConT ''Int))
        [FunD '(<>) [Clause [] (NormalB (VarE 'ok)) []]]]

stuff2 :: [Dec]
stuff2 =
  [InstanceD Nothing [AppT (ConT ''Num) a] (AppT (ConT ''Num) (AppT ListT a))
    [ValD (VarP 'fromInteger) (NormalB (InfixE (Just (VarE 'pure)) (VarE '(.)) (Just (VarE 'fromInteger)))) []]]
  where a = VarT $ Name (OccName "a") (NameU 0)

