{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DeriveFunctor #-}

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Identity
--import Control.Applicative
--import Text.Show.Functions

main :: IO ()
main = putStrLn "ciao"

--newtype MT1 m a = MT1 { runMT1 :: m (Maybe a) }

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
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in IntVal (i1 + i2)
eval0 env (Lambda argname body) = FunVal argname body env
eval0 env (App e1 e2) = let Lambda argname body = e1
                            envPlus = Map.insert argname (eval0 env e2) env
                        in eval0 envPlus body
--eval0 env (App (Lambda argname body) expr) = eval0 (Map.insert argname (eval0 env expr) env) body

lambda = Lambda "x" (Plus (Var "x") (Plus (Var "y") (Lit 1)))
envo = Map.insert "y" (IntVal 123) Map.empty

eval1 :: Env -> Exp -> Identity Value
eval1 _ (Lit i) = return (IntVal i)
eval1 env (Var name) = return $ fromJust $ Map.lookup name env
eval1 env (Plus e1 e2) = do
                             IntVal i1 <- eval1 env e1
                             IntVal i2 <- eval1 env e2
                             return $ IntVal (i1 + i2)
eval1 env (Lambda argname body) = return $ FunVal argname body env
eval1 env (App e1 e2) = let Lambda argname body = e1
                            envPlus = Map.insert argname (runIdentity (eval1 env e2)) env
                        in eval1 envPlus body
