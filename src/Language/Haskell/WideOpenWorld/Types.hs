{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Haskell.WideOpenWorld.Types where

import           Control.Arrow (first, second)
import           Control.Monad.State
import qualified Data.Map as M


data WowType
  = TyVar Int
  | TyApp Ctor [WowType]
  deriving (Show, Eq, Ord)


type Ctor = String

newtype TI s i o a = TI
  { runTI' :: State (s, M.Map i o) a
  } deriving (Functor, Applicative, Monad)

unify :: Ord i => i -> o -> TI s i o ()
unify i = TI . modify . second . M.insert i

fresh :: (s -> s) -> TI s i o s
fresh f = TI $ gets fst <* modify (first f)

findOrFresh :: Ord i => (o -> o) -> i -> TI o i o o
findOrFresh f i = do
  z <- TI . gets $ M.lookup i . snd
  case z of
    Just o -> pure o
    Nothing -> do
      o <- fresh f
      pure o


runTI :: Ord i => s -> TI s i o a -> a
runTI s (TI m) = evalState m (s, mempty)

