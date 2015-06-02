{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
import Data.Maybe
import qualified Data.Map as Map
import Control.Applicative
import Text.Show.Functions

type Name = String
data Exp = Lit Integer
          | Var Name
          | Plus Exp Exp
          | Lambda Name Exp
          | App Exp Exp
          deriving Show
data Value = IntVal Integer
           | FunVal Name Exp Env
           deriving Show
type Env = Map.Map Name Value

-- Expressions for exercises
-- var xxxx, yyyy
watIsXxxx = Var "xxxx"
watIsYyyy = Var "yyyy"
two_vars_env = Map.insert "xxxx" (IntVal 123) (Map.insert "yyyy" (IntVal 234) Map.empty)
xPlusY = Plus (Var "xxxx") (Var "yyyy")
-- \x -> x
lambdina = Lambda "x" (Var "x")
-- 12 + (\x -> x)(4 + 2)
sample = Plus (Lit 12) (App lambdina (Plus (Lit 4) (Lit 2)))

----------------------------------------------------------------------
eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var name) = fromJust $ Map.lookup name env
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in IntVal (i1 + i2)
eval0 env (Lambda argname body) = FunVal argname body env
eval0 env (App e1 e2) = let FunVal argname body env' = eval0 env e1
                            v2 = eval0 env e2
                        in eval0 (Map.insert argname v2 env') body
-----------------------------------------------------------------------

newtype I a = I a
  deriving Show
unI :: I a -> a
unI (I a) = a
instance Monad I where
  return = I
  ia >>= faib = faib (unI ia)
instance Applicative I where
  pure = I
  -- af(a->b) <*> afa :: afb
  ifab <*> ia = I (unI ifab (unI ia))
instance Functor I where
  fmap fab ia = I (fab (unI ia))
-----------------------------------------------------------------------
runEval1 :: I Value -> Value
runEval1 = unI

eval1 :: Env -> Exp -> I Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust $ Map.lookup n env
eval1 env (Plus e1 e2) = do (IntVal i1) <- eval1 env e1
                            (IntVal i2) <- eval1 env e2
                            return $ IntVal $ i1 + i2
eval1 env (Lambda argname body) = return $ FunVal argname body env
{-
eval1 env (App e1 e2) = do (FunVal argname body env') <- eval1 env e1
                           v2 <- eval1 env e2
                           eval1 (Map.insert argname v2 env') body
-}
eval1 env (App e1 e2) = let (FunVal argname body env') = unI $ eval1 env e1
                            v2 = unI $ eval1 env e2
                        in eval1 (Map.insert argname v2 env') body
----------------------------------------------------------------------
-- MaybeT, actually...
newtype ET m a = ET (m (Maybe a))

unET (ET m) = m

instance (Monad m) => Monad (ET m) where
  return a = ET (return (Just a))
  etma >>= faetmb = ET $ do maybea <- unET etma
                            case maybea of
                              Just a -> unET (faetmb a)
                              Nothing -> return Nothing

instance (Functor m, Monad m) => Applicative (ET m) where
  pure a = return a
  -- af (a -> b) <*> af a :: af b
  etmfab <*> etma = ET $ do maybefab <- (unET etmfab) -- etmb = ET(m(Maybe b))
                            maybea <- (unET etma)
                            return (maybefab <*> maybea)

instance (Functor m, Monad m) => Functor (ET m) where
  fmap fab etma = ET $ do maybea <- (unET etma) -- etmb = ET(m(Maybe b))
                          return ((Just fab) <*> maybea)

-- MonadError
class (Monad m) => E m where
  fail :: m a
  handle :: m a -> m a -> m a

instance (Monad m) => E (ET m) where
  fail = ET (return Nothing)
  handle tryclause catchclause = ET $ do maybetry <- unET tryclause
                                         case maybetry of
                                           Just _ -> return maybetry
                                           Nothing -> unET catchclause

------------------------------------------------------------------------
--runEval2 :: ET I Value -> Value
--runEval2 etia = unI (unET etia)

eval2 :: Env -> Exp -> ET I Value
eval2 env (Lit i) = return $ IntVal i
