{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DeriveFunctor #-}
module Nilsson_03 where -- ReaderT

  import Data.Maybe
  import Data.Monoid
  import qualified Data.Map as Map
  import Control.Applicative
  import Text.Show.Functions
  import Nilsson_01
  import Nilsson_02
  
  newtype RT r m a = RT (r -> m a)
  deriving instance Show (r -> m a) => Show (RT r m a)
  deriving instance Eq (r -> m a) => Eq (RT r m a)
  
  unRT :: (Monad m) => RT r m a -> r -> m a
  unRT (RT rma) = rma
  
  runRT :: (Monad m) => RT r m a -> r -> m a
  runRT = unRT
  
  instance (Monad m) => Monad (RT r m) where
    return x = RT $ \_ -> return x
    rtrma >>= fartrmb = RT $ \r -> do a <- unRT rtrma r
                                      unRT (fartrmb a) r

    {-
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

---------------------------------------------
  -- kind of MonadWriter
  class (Monoid w, Monad m) => W w m | m -> w where
    -- tell w is an action that produces the output w
    tell :: w -> m ()
    -- listen m is an action that executes action m and adds its output to the value of the computation
    listen :: m a -> m (a, w)
    -- pass m is an action that executes action m (which returns a value and a function) and returns the value applying the function to the output
    -- pass :: m (a, w -> w) -> m a 

  instance (Monoid w, Monad m) => W w (WT w m) where
    --tell :: (Monoid w, Monad m) => w -> WT w m ()
    tell msg = WT $ return ((), msg)
    --listen :: (Monoid w, Monad m) => WT w m a -> WT w m (a, w)
    listen ma = WT $ do (a, w) <- unWT ma
                        return ((a, w), w)
--    listen ma = do a <- ma
  --                 return (a, mempty)

  instance (Monoid w, Monad m) => MonadTransformer (WT w) m where
    -- lift :: m a -> t m a
    -- lift :: m a -> WT w m a = WT (m (a, w))
    lift ma = WT $ do a <- ma
                      return (a, mempty)

------------- ET + WT -----------------

  
  -}
  
  
--


