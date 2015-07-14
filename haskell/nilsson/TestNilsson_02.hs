
module TestNilsson_02 where 

  import Data.Maybe
  import qualified Data.Map as Map
  import Text.Show.Functions
  import Test.HUnit
  import Nilsson_01
  import Nilsson_02

  -- Expressions for exercises
  -- var xxxx, yyyy
  watIsXxxx = Var "xxxx"
  watIsYyyy = Var "yyyy"
  two_vars_env = Map.insert "xxxx" (IntVal 123) (Map.insert "yyyy" (IntVal 234) Map.empty)
  xPlusY = Plus (Var "xxxx") (Var "yyyy")
  -- \x -> x
  lambdina = Lambda "x" (Var "x")
  lambdona = Lambda "x" (Lambda "y" (Plus (Var "x") (Var "y")))
  -- 12 + (\x -> x)(4 + 2)
  sample = Plus (Lit 12) (App lambdina (Plus (Lit 4) (Lit 2))) -- IntVal 18
  samplone = App (App lambdona (Lit 4)) (Var "xxxx") -- IntVal (4 + xxxx)
----------------------------


  testEval3SimpleApp :: Test
  testEval3SimpleApp = 
      TestCase $ assertEqual "eval3 should make a simple application"
                             (Just (IntVal 18), 8) (runEval3 0 $ eval3 Map.empty sample)


  main :: IO Counts
  main = runTestTT $ TestList [

    testEval3SimpleApp
                               ]
