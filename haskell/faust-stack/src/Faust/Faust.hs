{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DeriveFunctor #-}

module Faust.Faust where

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Fail
import System.IO.Unsafe

--import Control.Applicative
--import Text.Show.Functions

instance MonadFail Identity where
    fail _ = undefined

--main :: IO ()
--main = putStrLn "ciao"

--newtype MT m a = MT (m (Maybe a))

--newtype RT e m a = RT { runRT :: e -> m a }

type Name = String

data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Lambda Name Exp
         | App Exp Exp -- App func arg
         deriving (Show, Read, Eq)

data Value = IntVal Integer
           | FunVal Name Exp Env
           deriving (Show, Read, Eq)

type Env = Map.Map Name Value

eval0 :: Env -> Exp -> Value
eval0 _ (Lit i) = IntVal i
eval0 env (Var name) = fromJust $ Map.lookup name env
eval0 env (Plus e1 e2) = let
    IntVal i1 = eval0 env e1
    IntVal i2 = eval0 env e2
  in IntVal (i1 + i2)
eval0 env (Lambda argname body) = FunVal argname body env
eval0 env (App lambda expr) = let
  v1 = eval0 env lambda
  v2 = eval0 env expr
  in case v1 of
    FunVal argname body env' -> eval0 (Map.insert argname v2 env') body
    _ -> undefined
--eval0 env (App (Lambda argname body) expr) = eval0 (Map.insert argname (eval0 env expr) env) body

-- newtype Reader r a = Reader { runReader :: r -> a }
eval0b :: Exp -> Reader Env Value
eval0b (Lit i) = return (IntVal i) -- useless: see next clause

eval0b exp = ask >>= (\env -> return $ eval0 env exp)
-- eval0b exp = do
--   env <- ask
--   return $ eval0 env exp

-- little mental masturbation; relevant impl is eval0b
eval0c :: Reader Env (Exp -> Value)
--eval0c = ask >>= (\env -> return $ eval0 env)
eval0c = do
  env <- ask
  return $ eval0 env

eval1 :: Env -> Exp -> Identity Value
eval1 _ (Lit i) = return (IntVal i)
eval1 env (Var name) = return $ fromJust $ Map.lookup name env
eval1 env (Plus e1 e2) = do
  IntVal i1 <- eval1 env e1
  IntVal i2 <- eval1 env e2
  return $ IntVal (i1 + i2)
eval1 env (Lambda argname body) = return $ FunVal argname body env
eval1 env (App lambda expr) = do
  v1 <- eval1 env lambda
  v2 <- eval1 env expr
  case v1 of
    FunVal argname body env' -> eval1 (Map.insert argname v2 env') body
    _ -> undefined

runEval1 :: Identity Value -> Value
runEval1 = runIdentity

-- newtype Reader r a = Reader { runReader :: r -> a }
-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
-- eval0b :: Exp -> Reader Env Value
type Eval5 a = ReaderT Env Identity a
eval5 :: Exp -> Eval5 Value
--              ReaderT Env (Identity Value)
--              ReaderT (Env -> Identity Value)
eval5 exp = do
  env <- ask
  return $ runIdentity $ eval1 env exp

--runEval2 :: MaybeT Identity Value -> Maybe Value -- Identity (Maybe Value)
--runEval5 :: Env -> ReaderT Env Identity Value -> Value
runEval5 :: Env -> Eval5 a -> a
runEval5 env eval5value  = runIdentity (runReaderT eval5value env)

eval1b :: Env -> Exp -> IO Value
eval1b _ (Lit i) = return (IntVal i)
eval1b env (Var name) = return $ fromJust $ Map.lookup name env
eval1b env (Plus e1 e2) = do
  IntVal i1 <- eval1b env e1
  IntVal i2 <- eval1b env e2
  return $ IntVal (i1 + i2)
eval1b env (Lambda argname body) = return $ FunVal argname body env
eval1b env (App lambda expr) = do
  v1 <- eval1b env lambda
  v2 <- eval1b env expr
  case v1 of
    FunVal argname body env' -> eval1b (Map.insert argname v2 env') body
    _ -> undefined

-- runEval1b :: IO Value -> Value
-- runEval1b = unsafePerformIO

--newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
eval2 :: Env -> Exp -> MaybeT Identity Value
eval2 _ (Lit i) = return (IntVal i)
eval2 env (Var name) = case Map.lookup name env of
  -- Just val -> return val -- MaybeT
  Just val -> MaybeT $ Identity $ Just val
  Nothing -> MaybeT (return Nothing) -- Identity
eval2 env (Plus e1 e2) = do
  v1 <- eval2 env e1
  v2 <- eval2 env e2
  case (v1, v2) of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> MaybeT $ return Nothing
eval2 env (Lambda argname body) = return $ FunVal argname body env
eval2 env (App lambda expr) = do
  v1 <- eval2 env lambda
  v2 <- eval2 env expr
  case v1 of
    FunVal argname body env' -> eval2 (Map.insert argname v2 env') body
    _ -> MaybeT (return Nothing)

runEval2 :: MaybeT Identity Value -> Maybe Value -- Identity (Maybe Value)
-- :t runMaybeT = MaybeT m a -> m (Maybe a)
-- :t runIdentity = Identity a -> a
runEval2 = runIdentity . runMaybeT

lambda = Lambda "x" (Plus (Var "x") (Plus (Var "y") (Lit 1)))
envo = Map.insert "y" (IntVal 123) Map.empty
