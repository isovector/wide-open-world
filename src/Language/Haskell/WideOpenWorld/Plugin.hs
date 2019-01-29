{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

module Language.Haskell.WideOpenWorld.Plugin
  ( plugin
  , stuff
  ) where

import           Control.Arrow (second)
import           Convert (convertToHsDecls)
import           CoreSyn
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Traversable
import           Data.Tuple (swap)
import           DsBinds
import           DsMonad
import           GHC (idType)
import           GHC.WhyArentYouExported
import           IOEnv
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
import           Unsafe.Coerce


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
        let w = head $ wanteds
        -- For the time being, just always return the instance corresponding to
        -- 'stuff2'.
        (bs, e, cts) <- unsafeTcPluginTcM $ buildInstance stuff2
        let (tys, _, inst) = tcSplitSigmaTy $ idType $ e
            mmap = match inst $ ctPred $ w
            instTys = (mmap M.!) <$> tys


        -- Emit a wanted for everything in the instance context. Keep track of
        -- their evidence vars.
        ctevs <- for (instantiateHead mmap <$> cts) . newWanted $ ctLoc w
        let ctevids = fmap (Var . ctEvEvId) ctevs

        -- Spit out the evidence as top level binds.
        for_ bs setEvBind

        pure $ TcPluginOk
          -- Give back evidence for the constraint, applying the wanted
          -- evidence.
          (fmap (EvExpr $ Var e `mkTyApps` instTys `mkApps` ctevids,) wanteds)
          (mkNonCanonical <$> ctevs)
     else pure $ TcPluginOk [] []
solveJDI g d w = pure $ TcPluginOk [] []


instantiateHead
    :: M.Map TyVar Type
    -> Type
    -> Type
instantiateHead mmap t =
  let (tc, tys) = splitAppTys t
      tys' = fmap (\ty -> maybe ty (mmap M.!) $ getTyVar_maybe ty) tys
   in mkAppTys tc tys'

localIOEnv :: (g -> g) -> IOEnv (Env g l) a -> IOEnv (Env g l) a
localIOEnv f m
  = unsafeCoerce
  . IOEnv'
  $ flip runIOEnv m
  . \env -> env { env_gbl = f $ env_gbl env }


getContext :: Kind -> ([TyVar], [PredType])
getContext k = (\(ts, a, _) -> (ts, a)) $ tcSplitSigmaTy k


buildInstance :: Dec -> TcM ([EvBind], Var, [PredType])
buildInstance z = localIOEnv clearTcGblEnv $ do
  let Right m = convertToHsDecls noSrcSpan [z]
  l <- tcRnSrcDecls m
  x <- initDsTc $ dsTopLHsBinds $ tcg_binds l
  let binds = drop 1 $ fromOL x
      dict = fst $ head binds
      evBinds = fmap (uncurry mkGivenEvBind . second EvExpr)
              $ binds
      (tys, cts) = getContext $ head $ fmap (idType . is_dfun) $ tcg_insts l
  pure (evBinds, dict, cts)


match
    :: PredType  -- class inst
    -> PredType  -- concrete type
    -> M.Map TyVar Type
match instClass concClass =
  let Just (_, instHead) = splitAppTy_maybe instClass
      Just (_, concHead) = splitAppTy_maybe concClass
      (_, instTys) = splitAppTys instHead
      (_, concTys) = splitAppTys concHead
   in M.fromList . mapMaybe (fmap swap . sequence . second getTyVar_maybe)
                 $ zip concTys instTys



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

