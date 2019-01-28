{-# OPTIONS_GHC -fplugin=Language.Haskell.WideOpenWorld.Plugin -dppr-debug -ddump-simpl -dsuppress-all #-}

module Lib where

test :: Int
test = 5 <> 6

