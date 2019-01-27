{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Language.Haskell.WideOpenWorld

cool [d| instance Semigroup Foo where (<>) _ _ = Foo |]

-- load [d| instance Num [boo] |]

-- test :: Foo
-- test = mempty

