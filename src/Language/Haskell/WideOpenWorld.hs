{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.WideOpenWorld where

import qualified Data.Map as M
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data Foo = Foo
  deriving Show


fooSemigroup :: Q [Dec]
fooSemigroup = [d|
  instance Semigroup Foo where
    (<>) _ _ = Foo
  |]


fooMonoid :: Q [Dec]
fooMonoid = [d|
  instance Monoid Foo where
    mempty = Foo
  |]


money :: M.Map (Name, Name) (Q [Dec])
money = M.fromList
  [ ((''Foo, ''Semigroup), fooSemigroup)
  , ((''Foo, ''Monoid),    fooMonoid)
  ]


loadInstance :: Name -> Name -> Q [Dec]
loadInstance tname iname = do
  maybe (error "no instance") id $ M.lookup (tname, iname) money

