
module TestNilsson_20 where 

  import Data.Maybe
  import qualified Data.Map as Map
  import Text.Show.Functions
  import Test.HUnit
  import Nilsson_01

  ex2a :: ST Int (ET I) Int
  ex2a = eHandle (sSet 3 >> eFail) sGet

  testRunST :: Test
  testRunST = 
      TestCase $ assertEqual "Nilsson's runST returns m a, not m (a, s)"
        (0) (runI $ runST sGet 0)

  testEx2a :: Test
  testEx2a = 
      TestCase $ assertEqual "Nilsson's runST returns m a, not m (a, s)"
        (0) (runI $ runET $ runST ex2a 0)

-------------------------------
  ex2b :: ET (ST Int I) Int
  ex2b = eHandle (sSet 3 >> eFail) sGet

  testRunET :: Test
  testRunET = 
      TestCase $ assertEqual "Nilsson's runET returns m a, not m (Maybe a)"
        (10) (runI $ runST (runET sGet) 10)

  testEx2b_run :: Test
  testEx2b_run = 
      TestCase $ assertEqual "Nilsson's runET saves the day in case of eFail ?!?!?!?!?!?"
        (3) (runI $ runST (runET ex2b) 0)

  testEx2b_un :: Test
  testEx2b_un = 
      TestCase $ assertEqual "Nilsson's unET returns (Maybe a, s)"
        (Just 3, 3) (unI $ unST (unET ex2b) 0)
{--}
  main :: IO Counts
  main = runTestTT $ TestList [
    testRunST,
    testEx2a,
    testRunET,
    testEx2b_run,
    testEx2b_un
                               ]
