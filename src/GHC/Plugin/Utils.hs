module GHC.Plugin.Utils where

import           Control.Arrow (second)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Tuple (swap)
import           IOEnv
import           TcRnTypes
import           TcType
import           Type
import           Unsafe.Coerce

------------------------------------------------------------------------------
-- | This constructor isn't exposed so I just copied it and 'unsafeCoerce' it
-- later. See 'localIOEnv'.
newtype IOEnv' env a = IOEnv' (env -> IO a)

localIOEnv :: (g -> g) -> IOEnv (Env g l) a -> IOEnv (Env g l) a
localIOEnv f m
  = unsafeCoerce
  . IOEnv'
  $ flip runIOEnv m
  . \env -> env { env_gbl = f $ env_gbl env }

instantiateHead
    :: M.Map TyVar Type
    -> Type
    -> Type
instantiateHead mmap t =
  let (tc, tys) = splitAppTys t
      tys' = fmap (\ty -> maybe ty (mmap M.!) $ getTyVar_maybe ty) tys
   in mkAppTys tc tys'

match
    :: PredType  -- class inst
    -> PredType  -- concrete type
    -> M.Map TyVar Type
match instClass concClass =
  let Just (_, instHead) = splitAppTy_maybe instClass
      Just (_, concHead) = splitAppTy_maybe concClass
      (_, instTys) = splitAppTys instHead
      (_, concTys) = splitAppTys concHead
   in M.fromList . mapMaybe (fmap swap . sequence . second getTyVar_maybe)
                 $ zip concTys instTys
