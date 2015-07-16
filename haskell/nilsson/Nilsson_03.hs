{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DeriveFunctor, InstanceSigs #-}
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

  instance (Functor m, Monad m) => Applicative (RT r m) where
    pure = return
    -- af (a -> b) <*> af a -> af b
    rtrmfab <*> rtrma = RT $ \r -> do fab <- unRT rtrmfab r
                                      a <- unRT rtrma r
                                      return (fab a)
  
    
  instance (Functor m, Monad m) => Functor (RT r m) where
    fmap fab rtrma = RT $ \r -> do a <- unRT rtrma r
                                   return (fab a)
---------------------------------------------
  -- kind of MonadReader
  class (Monad m) => R r m | m -> r where
    -- ask retrieves the monad environment
    ask :: m r
    -- executes ONE computation in a modified environment
    -- (r -> r) = the function that modifies the environment
    local :: (r -> r) -> m a -> m a

  instance (Monad m) => R r (RT r m) where
    ask :: (RT r m) r
    --ask = RT (\x -> x) r -- my attempt
    ask = RT return -- from hackage

    local :: (r -> r) -- ^ The function to modify the environment
        -> (RT r m) a -- ^ Computation to run in the modified the environment
        -> (RT r m) a
    local f m = RT $ unRT m . f
    -- local modifier rtrma = RT $ do env <- ask -- my attempt
       --                           return $

                                  {-
  instance (Monoid w, Monad m) => MonadTransformer (WT w) m where
    -- lift :: m a -> t m a
    -- lift :: m a -> WT w m a = WT (m (a, w))
    lift ma = WT $ do a <- ma
                      return (a, mempty)

------------- ET + WT -----------------

  
  -}
  
  
--


