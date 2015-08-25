
module Mycont_01 where

  import Control.Monad.Cont
  
  fact :: Int -> Cont r Int
  -- Your own code:
  -- Cont ($ (fact 0)) = return 1
  fact 0 = return 1
  -- Cont ($ (fact n)) = Cont ($ (fact (n-1))) >>= (\x -> Cont ($ (n*x)))
  fact n = fact (n-1) >>= \x -> return (n*x)

  -- the "real" factorial function, without monads
  factorial :: Int -> Int
  factorial n = runCont (fact n) id