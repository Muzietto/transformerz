{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
import Control.Applicative

class (Monad m, Monad (t m)) => MonadTransformer t m where
  lift :: m a -> t m a

class (Monad m) => E m where
  eFail :: m a
  eHandle :: m a -> m a -> m a

-- nearly MonadState
class (Monad m) => S m s | m -> s where
  sSet :: s -> m()
  sGet :: m s

-- identity
newtype I a = I a -- I { runI :: a }
unI (I a) = a

instance Monad I where
  return = I
  ia >>= faIa = faIa (unI ia)

instance Applicative I where
  pure = I
  -- af (a -> b) <*> af a :: af b
  ifab <*> ia = I (unI ifab (unI ia))

instance Functor I where
  fmap fab ia = I (fab (unI ia))

runI :: I a -> a
runI = unI

newtype ET m a = ET (m (Maybe a))
unET (ET m) = m

instance (Monad m) => Monad (ET m) where 
  return a = ET (return (Just a))
  etma >>= faETmb = ET $ do ma <- unET etma
                            case ma of
                              Just a -> unET (faETmb a)
                              Nothing -> return Nothing

instance (Applicative m, Monad m) => Applicative (ET m) where 
  pure a = return a
  -- af (a -> b) <*> af a :: af b
  etmfab <*> etma = ET $ do maybefab <- (unET etmfab) -- etmb = ET(m(Maybe b))
                            maybea <- (unET etma)
                            return (maybefab <*> maybea)

instance (Functor m, Monad m) => Functor (ET m) where
  fmap fab etma = ET $ do maybea <- (unET etma) -- etmb = ET(m(Maybe b))
                          return ((Just fab) <*> maybea)

-- ET m a = ET (m (Maybe a))
runET :: (Monad m) => ET m a -> m a
runET etma = do ma <- unET etma
                case ma of
                  Just a -> return a

instance (Monad m) => MonadTransformer ET m where
  lift m = ET (m >>= \a -> return (Just a))

instance (Monad m) => E (ET m) where
  eFail = ET (return Nothing)
  m1 `eHandle` m2 = ET $ do ma <- unET m1
                            case ma of
                              Just _ -> return ma
                              Nothing -> unET m2

