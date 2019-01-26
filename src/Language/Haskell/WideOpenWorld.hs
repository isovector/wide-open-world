{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Language.Haskell.WideOpenWorld where

import           Data.Bytes.Serial
import           Data.Bytes.Put
import           Data.Bytes.Get
import qualified Data.Map as M
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.ByteString.Lazy.Char8 as B

deriving instance Serial AnnTarget
deriving instance Serial Bang
deriving instance Serial Body
deriving instance Serial Callconv
deriving instance Serial Clause
deriving instance Serial Con
deriving instance Serial Dec
deriving instance Serial DerivClause
deriving instance Serial DerivStrategy
deriving instance Serial Exp
deriving instance Serial FamilyResultSig
deriving instance Serial Fixity
deriving instance Serial FixityDirection
deriving instance Serial Foreign
deriving instance Serial FunDep
deriving instance Serial Guard
deriving instance Serial InjectivityAnn
deriving instance Serial Inline
deriving instance Serial Lit
deriving instance Serial Match
deriving instance Serial ModName
deriving instance Serial NameFlavour
deriving instance Serial NameSpace
deriving instance Serial OccName
deriving instance Serial Overlap
deriving instance Serial Pat
deriving instance Serial PatSynArgs
deriving instance Serial PatSynDir
deriving instance Serial Phases
deriving instance Serial PkgName
deriving instance Serial Pragma
deriving instance Serial Range
deriving instance Serial Role
deriving instance Serial RuleBndr
deriving instance Serial RuleMatch
deriving instance Serial Safety
deriving instance Serial SourceStrictness
deriving instance Serial SourceUnpackedness
deriving instance Serial Stmt
deriving instance Serial TyLit
deriving instance Serial TySynEqn
deriving instance Serial TyVarBndr
deriving instance Serial Type
deriving instance Serial TypeFamilyHead

deriving instance Serial Name

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


inMemory :: M.Map (Name, Name) (Q [Dec])
inMemory = M.fromList
  [ ((''Foo, ''Semigroup), fooSemigroup)
  , ((''Foo, ''Monoid),    fooMonoid)
  ]

initializeDatabase :: Q [Dec]
initializeDatabase = do
  x <- sequenceA inMemory
  runIO $ B.writeFile "/home/sandy/wow.db" $ runPutL $ serialize x
  pure []


loadInstance :: Name -> Name -> Q [Dec]
loadInstance tname iname = do
  db <- runIO $ runGetL deserialize <$> B.readFile "/home/sandy/wow.db"
  maybe (error "no instance") pure $ curry M.lookup tname iname db

