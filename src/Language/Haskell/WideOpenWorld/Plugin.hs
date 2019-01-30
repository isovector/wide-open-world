{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

module Language.Haskell.WideOpenWorld.Plugin
  ( plugin
  ) where

import           Control.Arrow (second)
import           Control.Monad (join)
import           Convert (convertToHsDecls)
import           CoreSyn
import           Data.Foldable
import qualified Data.Map as M
import           Data.Traversable
import           DsBinds
import           DsMonad
import           GHC (idType)
import           GHC.Plugin.Utils
import           GHC.WhyArentYouExported
import           InstEnv
import           Language.Haskell.TH hiding (Type, ppr, Kind, match)
import           Language.Haskell.TH.Syntax hiding (Type, Kind)
import           Language.Haskell.WideOpenWorld.Test
import           OrdList
import           Outputable hiding ((<>))
import           Plugins (Plugin (..), defaultPlugin)
import           SrcLoc (noSrcSpan)
import           TcEvidence
import           TcPluginM
import           TcRnTypes
import           TcType
import           Type


plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = const $ pure $ TcPlugin
    { tcPluginInit  = pure ()
    , tcPluginSolve = const solve
    , tcPluginStop  = const $ pure ()
    }
  }


findDec :: PredType -> Maybe Dec
findDec = const $ pure stuff2


------------------------------------------------------------------------------
-- | The main solver.
solve :: [Ct]
      -> [Ct]
      -> [Ct]
      -> TcPluginM TcPluginResult
solve _ _ wanteds = do
  (new, solved) <- fmap sequenceA . for wanteds $ \w -> do
    (bs, e, cts) <- unsafeTcPluginTcM $ buildInstance stuff2

    let (tys, _, inst) = tcSplitSigmaTy $ idType e
        mmap = match inst $ ctPred $ w
        instTys = (mmap M.!) <$> tys

    -- Emit a wanted for everything in the instance context. Keep track of
    -- their evidence vars.
    ctevs <- for (instantiateHead mmap <$> cts) . newWanted $ ctLoc w
    let ctevids = fmap (Var . ctEvEvId) ctevs

    -- Spit out the evidence as top level binds.
    for_ bs setEvBind
    pure ( mkNonCanonical <$> ctevs
           -- Give back evidence for the constraint, applying the wanted
           -- evidence.
         , (EvExpr $ Var e `mkTyApps` instTys `mkApps` ctevids, w)
         )
  pure $ TcPluginOk solved new


getContext :: Kind -> [PredType]
getContext k = (\(_, a, _) -> a) $ tcSplitSigmaTy k


buildInstance :: Dec -> TcM ([EvBind], Var, [PredType])
buildInstance z = localIOEnv clearTcGblEnv $ do
  let Right m = convertToHsDecls noSrcSpan [z]
  l <- tcRnSrcDecls m
  x <- initDsTc $ dsTopLHsBinds $ tcg_binds l
  let binds = drop 1 $ fromOL x
      dict = fst $ head binds
      evBinds = fmap (uncurry mkGivenEvBind . second EvExpr)
              $ binds
      cts = getContext $ head $ fmap (idType . is_dfun) $ tcg_insts l
  pure (evBinds, dict, cts)


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

