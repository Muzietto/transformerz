{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DeriveFunctor #-}
module Nilsson_02 where

  import Data.Maybe
  import Data.Monoid
  import qualified Data.Map as Map
  import Control.Applicative
  import Text.Show.Functions
  import Nilsson_01
  
  newtype WT w m a = WT (m (a, w))
  deriving instance Show (m (a, w)) => Show (WT w m a)
  deriving instance Eq (m (a, w)) => Eq (WT w m a)
  
  unWT :: (Monoid w, Monad m) => WT w m a -> m (a, w)
  unWT (WT mcaw) = mcaw
  
  runWT :: (Monoid w, Monad m) => WT w m a -> m a
  runWT mcaw = do (aa, _) <- unWT mcaw
                  return aa
  
  instance (Monoid w, Monad m) => Monad (WT w m) where
    return x = WT (return (x, mempty))
    wtmcaw >>= fawtwmb = WT $ do (a, w1) <- unWT wtmcaw
                                 (b, w2) <- unWT (fawtwmb a)
                                 return (b, mappend w1 w2)

  instance (Monoid w, Functor m, Monad m) => Applicative (WT w m) where
    pure = return
    -- af (a -> b) <*> af a -> af b
    -- (a -> b, w) <*> (a, w) -> (b, w)
    wtwmfab <*> wtwma = WT $ do (fab, w1) <- unWT wtwmfab 
                                (a, w2) <- unWT wtwma
                                return (fab a, mappend w1 w2)
  
  instance (Monoid w, Functor m, Monad m) => Functor (WT w m) where
    fmap fab wtwma = WT $ do (a, w) <- unWT wtwma
                             return (fab a, w)
