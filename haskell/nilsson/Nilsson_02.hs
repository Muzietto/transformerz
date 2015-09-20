{-
	TRANSFORMERZ - Monad Transformer in vanilla Haskell
  Nothing imported - just code
	Author: Marco Faustinelli (contacts@faustinelli.net)
	Web: http://faustinelli.net/
	     http://faustinelli.wordpress.com/
	Version: 1.0

	The MIT License - Copyright (c) 2015 Transformerz Project
-}

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DeriveFunctor #-}
module Nilsson_02 where -- WriterT

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

  type Eval4c a = ET (WT [String] I) a
             -- = ET (WT [String] I) (Maybe a)
             -- = ET (WT I(Maybe a, [String]))
          
  runEval4c :: Eval4c Value -> (Maybe Value, [String])
  runEval4c etwticmaybevalstrarra = 
            unI $ unWT $ unET etwticmaybevalstrarra
            
  eval4c :: Env -> Exp -> Eval4c Value
  eval4c env (Lit i) = do lift $ tell ["literal"]
                          return $ IntVal i
  eval4c env (Var name) = do lift $ tell ["lookup " ++ name]
                             case (Map.lookup name env) of
                               Just val -> do lift $ tell ["lookup ok"]
                                              return $ val
                               Nothing -> do lift $ tell ["lookup ko"]
                                             eFail
  eval4c env (Plus e1 e2) = 
       do lift $ tell ["sum"]
          v1 <- eval4c env e1
          v2 <- eval4c env e2
          case (v1, v2) of
            (IntVal i1, IntVal i2) -> do lift $ tell ["sum ok"]
                                         return $ IntVal (i1 + i2)
            _ -> do lift $ tell ["sum ko"]
                    eFail
  eval4c env (Lambda argname body) =
       do lift $ tell ["lambda"]
          return $ FunVal argname body env
  eval4c env (App lambda exp) =
       do lift $ tell ["application"]
          val <- eval4c env exp
          funval <- eval4c env lambda
          case funval of
            FunVal argname body env' -> do lift $ tell ["application ok"]
                                           eval4c (Map.insert argname val env') body
            _ -> do lift $ tell ["application ko"]
                    eFail

------------- ET + WT + ST -----------------

  type Eval4d s a = ET (WT [String] (ST s I)) a
               -- = ET (WT [String] (ST s I)) (Maybe a)
               -- = ET (WT (ST s I)(Maybe a, [String]))
               -- = ET (WT (ST (s -> I((Maybe a, [String]), s))))

  runEval4d :: Int -> Eval4d Int Value -> ((Maybe Value, [String]), Int)
  runEval4d s etwtstinticcmaybevalstrarraint = 
              unI $ unST (unWT (unET etwtstinticcmaybevalstrarraint)) s

  eval4d :: Env -> Exp -> Eval4d Int Value
  eval4d env (Lit i) = do lift $ lift tick
                          lift $ tell ["literal"]
                          return $ IntVal i
  eval4d env (Var name) = do lift $ lift tick
                             lift $ tell ["lookup " ++ name]
                             case (Map.lookup name env) of
                               (Just val) -> do lift $ tell ["lookup ok"]
                                                return $ val
                               Nothing -> do lift $ tell ["lookup ko"]
                                             eFail
  eval4d env (Plus e1 e2) = do lift $ lift tick
                               lift $ tell ["sum"]
                               v1 <- eval4d env e1
                               v2 <- eval4d env e2
                               case (v1, v2) of
                                 (IntVal i1, IntVal i2) -> do lift $ tell ["sum ok"]
                                                              return $ IntVal $ i1 + i2
                                 _ -> do lift $ tell ["sum ko"]
                                         eFail
  eval4d env (Lambda argname body) =
       do lift $ lift tick
          lift $ tell ["lambda"]
          return $ FunVal argname body env
  eval4d env (App lambda exp) =
       do lift $ lift tick
          lift $ tell ["application"]
          val <- eval4d env exp
          funval <- eval4d env lambda
          case funval of
            FunVal argname body env' -> do lift $ tell ["application ok"]
                                           eval4d (Map.insert argname val env') body
            _ -> do lift $ tell ["application ko"]
                    eFail
  
  
  
  
--


