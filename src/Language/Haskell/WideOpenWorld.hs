{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Language.Haskell.WideOpenWorld where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Bytes.Get
import           Data.Bytes.Put
import           Data.Bytes.Serial
import           Data.IORef
import qualified Data.Map as M
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import System.IO.Unsafe

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


dbRef :: IORef (Maybe (M.Map (Name, Name) [Dec]))
dbRef = unsafePerformIO $ newIORef Nothing
{-# NOINLINE dbRef #-}


getDB :: Q (M.Map (Name, Name) [Dec])
getDB = runIO $ do
  cached <- readIORef dbRef
  case cached of
    Just db -> pure db
    Nothing -> do
      resp <- simpleHTTP $ getRequest "http://reasonablypolymorphic.com/wow.db"
      body <- getResponseBody resp
      let db = runGetL deserialize $ B.pack $ body
      writeIORef dbRef $ Just db
      pure db


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
  db <- getDB
  maybe (error "no instance") pure $ curry M.lookup tname iname db

