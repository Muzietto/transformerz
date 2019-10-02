{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DeriveFunctor #-}

module Faust.Faust where

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Fail
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
    _ -> undefined --return Nothing

runEval1 :: Identity Value -> Value
runEval1 = runIdentity

--newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
eval2 :: Env -> Exp -> MaybeT Identity Value
eval2 _ (Lit i) = return (IntVal i)
eval2 env (Var name) = case (Map.lookup name env) of
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
