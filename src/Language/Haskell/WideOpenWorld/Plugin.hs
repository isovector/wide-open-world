{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -Wall         #-}

module Language.Haskell.WideOpenWorld.Plugin
  ( plugin
  , stuff
  ) where

import Control.Arrow (second)
import Convert (convertToHsDecls)
import CoreSyn
import Data.Foldable
import Data.Traversable
import DsBinds
import DsMonad
import GHC (idType)
import GHC.WhyArentYouExported
import IOEnv
import InstEnv
import Language.Haskell.TH hiding (Type, ppr, Kind)
import Language.Haskell.TH.Syntax hiding (Type, Kind)
import Language.Haskell.WideOpenWorld.Test
import OrdList
import Outputable hiding ((<>))
import Plugins (Plugin (..), defaultPlugin)
import SrcLoc (noSrcSpan)
import TcEvidence
import TcPluginM
import TcRnTypes
import TcType
import Type
import Unsafe.Coerce


plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const (Just jdiPlugin) }

jdiPlugin :: TcPlugin
jdiPlugin =
  TcPlugin { tcPluginInit  = pure ()
           , tcPluginSolve = const solveJDI
           , tcPluginStop  = const $ pure ()
           }


------------------------------------------------------------------------------
-- | This constructor isn't exposed so I just copied it and 'unsafeCoerce' it
-- later. See 'localIOEnv'.
newtype IOEnv' env a = IOEnv' (env -> IO a)


------------------------------------------------------------------------------
-- | The main solver.
solveJDI :: [Ct]
         -> [Ct]
         -> [Ct]
         -> TcPluginM TcPluginResult
solveJDI [] [] wanteds = do
  if (not $ null wanteds)
     then do
        -- For the time being, just always return the instance corresponding to
        -- 'stuff2'.
        (bs, e, _, cts) <- unsafeTcPluginTcM $ buildInstance stuff2

        -- Emit a wanted for everything in the instance context. Keep track of
        -- their evidence vars.
        ctevs <- for cts (newWanted $ ctLoc $ head wanteds)
        let ctevids = fmap (Var . ctEvEvId) ctevs

        -- Spit out the evidence as top level binds.
        for_ bs setEvBind

        pure $ TcPluginOk
          -- Give back evidence for the constraint, applying the wanted
          -- evidence.
          (fmap (EvExpr $ Var e `mkApps` ctevids,) wanteds)
          []
     else pure $ TcPluginOk [] []
solveJDI g d w = pprPanic "YO" $ ppr $ g ++ d ++ w


localIOEnv :: (g -> g) -> IOEnv (Env g l) a -> IOEnv (Env g l) a
localIOEnv f m
  = unsafeCoerce
  . IOEnv'
  $ flip runIOEnv m
  . \env -> env { env_gbl = f $ env_gbl env }


getContext :: Kind -> ([TyVar], [PredType])
getContext k = (\(ts, a, _) -> (ts, a)) $ tcSplitSigmaTy k


buildInstance :: Dec -> TcM ([EvBind], Var, [TyVar], [PredType])
buildInstance z = localIOEnv clearTcGblEnv $ do
  let Right m = convertToHsDecls noSrcSpan [z]
  l <- tcRnSrcDecls m
  x <- initDsTc $ dsTopLHsBinds $ tcg_binds l
  let binds = drop 1 $ fromOL x
      dict = fst $ head binds
      evBinds = fmap (uncurry mkGivenEvBind . second EvExpr)
              $ binds
      (tys, cts) = getContext $ head $ fmap (idType . is_dfun) $ tcg_insts l
  pure (evBinds, dict, tys, cts)


stuff :: Dec
stuff =
  InstanceD Nothing []
    (AppT (ConT ''Semigroup)
      (ConT ''Int))
        [FunD '(<>) [Clause [] (NormalB (VarE 'ok)) []]]

stuff2 :: Dec
stuff2 =
  InstanceD Nothing [AppT (ConT ''Num) a] (AppT (ConT ''Num) (AppT ListT a))
    [ValD (VarP 'fromInteger) (NormalB (InfixE (Just (VarE 'pure)) (VarE '(.)) (Just (VarE 'fromInteger)))) []]
  where a = VarT $ Name (OccName "a") (NameU 0)

