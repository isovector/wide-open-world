{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Language.Haskell.WideOpenWorld

load [d| instance Num [boo] |]

-- test :: Foo
-- test = mempty

