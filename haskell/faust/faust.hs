{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DeriveFunctor #-}

import Data.Maybe
import qualified Data.Map as Map
import Control.Applicative
import Text.Show.Functions

newtype MT1 m a = MT1 { runMT1 :: m (Maybe a) }

newtype MT m a = MT (m (Maybe a))

newtype RT e m a = RT { runRT :: e -> m a }

type Name = String
data Exp = Lit String
         | Var Name
         | Plus Exp Exp
         | Lambda Name Exp
         | App Exp Exp
         deriving (Show, Read, Eq)
data Value = IntVal Integer
           | FunVal Name Exp Env
           deriving (Show, Read, Eq)
type Env = Map.Map Name Value

eval0 :: Env -> Exp -> Value
