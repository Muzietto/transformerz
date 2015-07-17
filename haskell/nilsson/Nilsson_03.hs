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
    ask = RT return -- from hackage - why without DOT???

    local :: (r -> r) -- ^ The function to modify the environment
             -> (RT r m) a -- ^ Computation to run in the modified the environment
             -> (RT r m) a
    local f m = RT $ unRT m . f
 -- local modifier rtrma = RT $ do env <- ask -- my attempt
      --                           return $

  instance (Monad m) => MonadTransformer (RT r) m where
    -- lift :: m a -> t m a
    -- lift :: m a -> RT r m a = RT (r -> m a))
  --lift ma = RT $ \_ -> ma
    lift = RT . const
    
------------- RT + ET -----------------

  type Eval5 a = RT Env (ET I) a
            --  = RT (Env -> ET I a)
            --  = RT (Env -> ET I (Maybe a))
  
  runEval5 :: Env -> Eval5 a -> Maybe a
  runEval5 env rtenvetimaybea = unI $ unET $ unRT rtenvetimaybea env
  
  eval5 :: Exp -> Eval5 Value
  eval5 (Lit i) = return $ IntVal i
  eval5 (Var name) = do env <- ask
                        case (Map.lookup name env) of
                          Just val -> return val
                          Nothing -> lift eFail
  eval5 (Plus e1 e2) = do v1 <- eval5 e1
                          v2 <- eval5 e2
                          case (v1,v2) of
                            (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
                            _ -> lift eFail
  eval5 (Lambda argname body) = do env <- ask
                                   return $ FunVal argname body env
  eval5 (App lambda exp) =
        do funval <- eval5 lambda
           val <- eval5 exp
           case funval of
             FunVal argname body env' -> 
                  do local (const $ Map.insert argname val env') (eval5 body)
             _ -> lift eFail
  
------------- ET + RT -----------------

  type Eval5b a = ET (RT Env I) a
            -- = ET (RT Env I) (Maybe a)
            -- = ET (RT (Env -> I (Maybe a))
  
  runEval5b :: Env -> Eval5b a -> Maybe a
  runEval5b env etrtenvimaybea = unI $ unRT (unET etrtenvimaybea) env

  eval5b :: Exp -> Eval5b Value
  eval5b (Lit i) = return $ IntVal i
  eval5b (Var name) = do env <- lift ask
                         case (Map.lookup name env) of
                          Just val -> return val
                          Nothing -> eFail
  eval5b (Plus e1 e2) = do v1 <- eval5b e1
                           v2 <- eval5b e2
                           case (v1,v2) of
                             (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
                             _ -> eFail
  eval5b (Lambda argname body) = do env <- lift ask
                                    return $ FunVal argname body env
  eval5b (App lambda exp) =
        do funval <- eval5b lambda
           val <- eval5b exp
           case funval of
             FunVal argname body env' -> 
                  let body' = runET $ eval5b body
                  in do lift (local (const $ Map.insert argname val env') body')
             _ -> eFail
  
  {-
    -}
------------- RT + ET + ST -----------------
  
  --


