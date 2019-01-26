{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Language.Haskell.WideOpenWorld


loadInstance ''Foo ''Semigroup
loadInstance ''Foo ''Monoid

test :: Foo
test = mempty

