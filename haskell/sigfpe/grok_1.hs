import Control.Monad.State
--import Control.Monad.Identity
test1 :: State int int
test1 = do -- fuck
  a <- get
--  modify (+1)
-- b <- get
  return (a+1)