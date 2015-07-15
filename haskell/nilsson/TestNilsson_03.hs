
module TestNilsson_03 where 

  import Data.Maybe
  import qualified Data.Map as Map
  import Text.Show.Functions
  import Test.HUnit
  import Nilsson_01
  import Nilsson_02
  import Nilsson_03

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


  testEval4dLiteral :: Test
  testEval4dLiteral = 
      TestCase $ assertEqual "eval4d should evaluate a silly literal while updating state"
                             ((Just (IntVal 18), ["literal"]), 1) (runEval4d 0 $ eval4d Map.empty (Lit 18))
{--}


        
  main :: IO Counts
  main = runTestTT $ TestList [
                                
                                testEval4dLiteral
                              ]
