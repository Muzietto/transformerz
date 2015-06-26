{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DeriveFunctor #-}
import Data.Maybe
import qualified Data.Map as Map
import Control.Applicative
import Text.Show.Functions

class (Monad m, Monad (t m)) => MonadTransformer t m where
  lift :: m a -> t m a

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

-- unwrap the monad
unI :: I a -> a
unI (I a) = a

-- run the monad
runI :: I a -> a
runI = unI
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
deriving instance Show (m (Maybe a)) => Show (ET m a)

-- unwrap the OUTER monad, i.e. resolve its type
unET :: (Monad m) => ET m a -> m (Maybe a)
unET (ET m) = m

-- run the OUTER monad
runET :: Monad m => ET m a -> m a
runET etma = do ma <- unET etma
                case ma of 
                  Just a -> return a
--                Nothing -> crash!

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

-- kind of MonadError
class (Monad m) => E m where
  eFail :: m a
  eHandle :: m a -> m a -> m a

instance (Monad m) => E (ET m) where
  eFail = ET (return Nothing)
  eHandle tryclause catchclause = ET $ do maybetry <- unET tryclause
                                          case maybetry of
                                            Just _ -> return maybetry
                                            Nothing -> unET catchclause

instance (Monad m) => MonadTransformer ET m where
  -- (a -> m b) -> (a -> t m b), opp. m a -> t m a
  -- lift famb = \a -> ET $ do mb <- famb a
                            -- return mb
  -- m a -> t m a
  -- lift :: m a -> ET m a = ET m (Maybe a)
  lift ma = ET $ do a <- ma -- ET $ ma >>= \a -> return (Just a)
                    return (Just a) 

------------------------------------------------------------------------
runEval2 :: ET I Value -> Maybe Value -- ErrorT => Either String Value
runEval2 etia = unI (unET etia)

eval2 :: Env -> Exp -> ET I Value -- ET I (Maybe Value)
eval2 env (Lit i)      = return $ IntVal i
eval2 env (Var name)   = case (Map.lookup name env) of
                           Just v -> return v
                           Nothing -> eFail
eval2 env (Plus e1 e2) = do v1 <- eval2 env e1
                            v2 <- eval2 env e2
                            case (v1, v2) of
                              (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
                              _ -> eFail
eval2 env (Lambda argname body) = return $ FunVal argname body env
eval2 env (App e1 e2)           = do v1 <- eval2 env e1
                                     v2 <- eval2 env e2
                                     case v1 of
                                       FunVal argname body env' -> eval2 (Map.insert argname v2 env') body
                                       _ -> eFail

-------------------------------------------------------------------------
newtype ST s m a = ST (s -> m (a, s))
deriving instance Show (s -> m (a, s)) => Show (ST s m a)

-- unwrap the OUTER monad, i.e. resolve its type
unST :: (Monad m) => ST s m a -> s -> m (a, s)
unST (ST m) = m

-- run the OUTER monad
runST :: Monad m => ST s m a -> s -> m a
runST stsma = \s -> do (aaa, _) <- unST stsma s
                       return aaa


instance (Monad m) => Monad (ST s m) where
  return x = ST (\s -> return (x, s))
  stsma >>= fastsmb = ST $ \s -> do (aaa, sss) <- unST stsma s
                                    unST (fastsmb aaa) sss

instance (Functor m, Monad m) => Applicative (ST s m) where
  pure = return
  -- af (a -> b) <*> af a -> af b
  stsmfab <*> stsma = ST $ \s -> do (fab, sfab) <- unST stsmfab s
                                    (a, sa) <- unST stsma s
                                    return (fab a, sa) 

instance (Functor m, Monad m) => Functor (ST s m) where
  fmap fab stsma = ST $ \s -> do (a, sa) <- unST stsma s
                                 return (fab a, sa)

-- kind of MonadState
class (Monad m) => S m s | m -> s where
  sGet :: m s       -- I'd say s -> m (s, s))
  sSet :: s -> m () -- I'd say s -> _ -> m ((), s)

instance (Monad m) => S (ST s m) s where
  sGet   = ST $ \s -> return (s, s)
  sSet s = ST $ \_ -> return ((), s)

instance (Monad m) => MonadTransformer (ST s) m where
  -- lift :: m a -> t m a
  -- lift :: m a -> ST s m a = ST (s -> m (a,s))
  lift ma = ST $ \s -> do a <- ma -- ST $ \s -> ma >>= \a -> return (a, s)
                          return (a, s)
{-
-- MUTUAL TRANSFORMATIONS
-- an S monad transformed by ET is an S monad
instance (S m) => S (ET m) where
  -- sGet :: s -> (ET m) (s,s)
  sGet = 
  -- sSet :: s -> _ -> (ET m) ((),s)
  sSet = 
-}
-- en E monad transformed by ST is an E monad
instance (E m) => E (ST s m) where -- E (ST s m a) = E (ST (s -> m (a,s)))
  -- eFail :: (ST s m) a
  eFail = ST $ \s -> eFail -- = lift eFail
  -- eHandle :: (ST s m) a -> (ST s m) a -> (ST s m) a ||| REM unST :: (Monad m) => ST s m a -> s -> m (a, s)
  eHandle trystsma catchstsma = ST $ \s -> do tryas <- unST trystsma s -- running in m => tryas :: (a, s)
                                              catchas <- unST catchstsma s
                                              eHandle (return tryas) (return catchas) -- eHandle di m
-- ST $ \s -> eHandle_di_m (unST trystsma s) (unST catchstsma s) <-- VERIFY THIS!!!
-- eHandle_di_m :: m a -> m a -> m a
-- oppure: eHandle_di_m :: m (a,s) -> m (a,s) -> m (a,s) -- è come se dove c'è ST, il valore non è più :: a, ma :: (a,s)

----------------------------------------------------------------------------
type Eval3 s a = ST s (ET I) a -- = ST (s -> ET I (a, s)) = ST (s -> ET (I (Maybe (a, s))))

-- e3val s = (s, etia)
-- unST (ST m) = m
{-
  ex2a :: ST Int (ET I) Int
  ex2a= (sSet 3 >> eFail) ‘eHandle‘ sGet

  runI (runST (runET ex2b) 0)
-}

-- resolve this!!!
--runEval3 :: Integer -> ST s (ET I) Value -> (Maybe Value, Integer)
--runEval3 s e3val = do (etiv, s') <- unST e3val s
--                      return (unI (unST etiv), s')

--runEval2 :: ET I Value -> Maybe Value
--runEval2 etia = unI (unET etia)

eval3 :: Env -> Exp -> Eval3 Int Value
eval3 env (Lit i) = return $ IntVal i

pippo = eval3 Map.empty (Lit 123)
pluto = unST pippo 0

{-
  HOW TO USE values produced by eval3 :: ST Int (ET I) Value
  unST e3val 0 :: ET I (Value, Int)
  
  pippo = eval3 Map.empty (Lit 123) 0
  pippo :: ST Int (ET I) Value = ST (s -> ET I (Value, Int)) = ST (s -> ET (I (Maybe (Value, Int))))
  
  unST pippo 0 :: ET I (Value, Int) = ET (I (Maybe (Value, Int)))
  unST pippo 0 = ET (I (Just (IntVal 123, 0)))
  
  unI $ unET $ unST pippo 0 = Just(IntVal 123, 0)
-}


