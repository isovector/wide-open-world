{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall      #-}

module Main where

import           Control.Monad
import           Control.Monad.Free
import           Data.Functor.Compose
import qualified Data.Map as M
import           Data.Maybe (maybeToList)
import           Language.Haskell.WideOpenWorld.Types
import           Servant
import           Servant.Server

data family Base t :: * -> *

data instance Base WowType a where
  TyVarF :: Int         -> Base WowType a
  TyAppF :: Ctor -> [a] -> Base WowType a


type SearchTree' a = Free (Base WowType `Compose` []) a



data SearchTree a
  = Leaf Int a
  | Branch (Maybe (Int, a))
           (M.Map Ctor [SearchTree a])


search
    :: WowType
    -> SearchTree a
    -> [a]
search (TyVar v1) (Leaf v2 a)
  | v1 == v2 = [a]
  | otherwise = []
search (TyVar v1) (Branch (Just (v2, a)) _)
  | v1 == v2 = [a]
  | otherwise = []
search (TyVar _) (Branch Nothing _)
  = []
search (TyApp _ _) (Leaf _ a)
  = [a]  -- todo: care about unification
search (TyApp ct ts) (Branch ok bs)
  = mappend (fmap snd $ maybeToList ok)
  . join
  . zipWith search ts
  . maybe [] id
  $ M.lookup ct bs


main :: IO ()
main = putStrLn ""

