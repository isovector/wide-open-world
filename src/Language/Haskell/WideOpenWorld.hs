{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Language.Haskell.WideOpenWorld where

import           Control.Arrow (first, second)
import           Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Bytes.Get
import           Data.Bytes.Put
import           Data.Bytes.Serial
import           Data.IORef
import qualified Data.Map as M
import           Data.Traversable
import           Generics.SYB hiding (Fixity (..))
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import           System.IO.Unsafe

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


dbRef :: IORef (Maybe (M.Map Type [Dec]))
dbRef = unsafePerformIO $ newIORef Nothing
{-# NOINLINE dbRef #-}


getDB :: Q (M.Map Type [Dec])
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


numList :: Q [Dec]
numList = [d|
  instance Num [a] where
    fromInteger _ = []
  |]

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


instanceHead :: Dec -> Q Type
instanceHead (InstanceD _ _ t _) = pure t
instanceHead x = fail $ "Not an instance declaration: " ++ show x


normalizedName :: Int -> Name
normalizedName n = Name (OccName "wow") (NameU n)


normalizeHead :: Type -> Type
normalizeHead = flip evalState (0, M.empty)
              . everywhereM (mkM renameU)
  where
    renameU (Name (OccName _) (NameU n)) = do
      m <- gets snd
      case M.lookup n m of
        Just z -> pure $ normalizedName z
        Nothing -> do
          z <- gets fst
          modify $ first (+1)
          modify . second $ M.insert n z
          pure $ normalizedName z
    renameU z = pure z


sig1 :: Q [Dec] -> Q Type
sig1 = (fmap head . traverse (fmap normalizeHead . instanceHead) =<<)


inMemory' :: Q (M.Map Type (Q [Dec]))
inMemory' = do
  fooSemiT   <- sig1 [d| instance Semigroup Foo |]
  fooMonoidT <- sig1 [d| instance Monoid Foo |]
  numListT   <- sig1 [d| instance Num [a] |]
  pure $ M.fromList
    [ (fooSemiT,   fooSemigroup)
    , (fooMonoidT, fooMonoid)
    , (numListT,   numList)
    ]


initializeDatabase :: Q [Dec]
initializeDatabase = do
  x <- sequenceA =<< inMemory'
  runIO $ B.writeFile "/home/sandy/wow.db" $ runPutL $ serialize x
  pure []


load :: Q [Dec] -> Q [Dec]
load instsDQ = do
  db <- getDB
  nheads <- traverse (fmap normalizeHead . instanceHead) =<< instsDQ
  pure . join . for nheads $ (db M.!)

