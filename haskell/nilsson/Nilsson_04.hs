{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DeriveFunctor, InstanceSigs #-}

module Nilsson_04 where -- ListT

  import Data.List
  import Data.Maybe
  import Data.Monoid
  import qualified Data.Map as Map
  import Control.Applicative
  import Text.Show.Functions
  import Nilsson_01
  import Nilsson_02
  import Nilsson_03

  mapMarco :: (Monad m) => (a -> m b) -> [a] -> m [b]
  mapMarco famb as  = let mb_s = map famb as
                      in mb_s >>= (\mb -> do b <- mb
                                             return (concat [[b]]))

  newtype AT m a = AT (m [a])
  deriving instance Show (m [a]) => Show (AT m a)
  deriving instance Eq (m [a]) => Eq (AT m a)

  unAT :: AT m a -> m [a]
  unAT (AT ama) = ama

  runAT :: AT m a -> m [a]
  runAT = unAT

  {-
  instance (Monad m) => Monad (AT m) where
    return x = AT (return [x])
    atma >>= faatmb = do la <- unAT atma
                         a <- la
                         a
                         
                         -}