{-# OPTIONS_GHC -fplugin=Language.Haskell.WideOpenWorld.Plugin -dcore-lint #-}

module Lib where


-- instance Semigroup Int where
--   (<>) = ok
--   {-# NOINLINE (<>) #-}

test :: [Int]
test = 5

