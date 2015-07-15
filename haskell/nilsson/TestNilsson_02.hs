
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


  testEval4cLiteral :: Test
  testEval4cLiteral = 
      TestCase $ assertEqual "eval4c should evaluate a silly literal"
                             (Just (IntVal 18), ["literal"]) (runEval4c $ eval4c Map.empty (Lit 18))
{-
  testEval3SimpleApp :: Test
  testEval3SimpleApp = 
      TestCase $ assertEqual "eval3 should make a simple application"
                             (Just (IntVal 18), 8) (runEval3 0 $ eval3 Map.empty sample)

  testEval3ComplexApp :: Test
  testEval3ComplexApp = 
      TestCase $ assertEqual "eval3 should make a complex application"
                             (Just (IntVal 127), 9) (runEval3 0 $ eval3 two_vars_env samplone)

  testEval3CurriedApp :: Test
  testEval3CurriedApp = 
      TestCase $ assertEqual "eval3 should make a partial application"
                             (Just (FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)])), 4) 
                             (runEval3 0 $ eval3 Map.empty (App lambdona (Lit 4)))

  testEval3CurriedApp2 :: Test
  testEval3CurriedApp2 = 
      TestCase $ assertEqual "eval3 should spit Nothing in case of errore"
                             (Nothing, 3) 
                             (runEval3 0 $ eval3 Map.empty (App lambdona (Var "inesistente")))
-}
  testEval4cWatIsXPlusY :: Test
  testEval4cWatIsXPlusY = 
      TestCase $ assertEqual "eval4c should sum two vars"
                             (Just (IntVal 357), ["sum",
                                                  "lookup xxxx",
                                                  "ok",
                                                  "lookup yyyy",
                                                  "ok",
                                                  "ok"
                                                  ]) (runEval4c $ eval4c two_vars_env xPlusY)

  testEval4cWatIsXPlusCrash :: Test
  testEval4cWatIsXPlusCrash = 
      TestCase $ assertEqual "eval4c should fail summing stuff when one ain't IntVal"
                             (Nothing, ["sum",
                                       "literal",
                                       "lambda",
                                       "ko"
                             ]) (runEval4c $ eval4c Map.empty (Plus (Lit 123) lambdina))
        
  testEval4cVarUndefined :: Test
  testEval4cVarUndefined = 
      TestCase $ assertEqual "eval4c should fail on non-existing vars and write about it"
        (Nothing, ["lookup zzzz","ko"])
        (runEval4c (eval4c two_vars_env (Var "zzzz")))

  testEval4cVarXxxx :: Test
  testEval4cVarXxxx = 
      TestCase $ assertEqual "eval4c should lookup a Var and write about it"
        (Just (IntVal 123), ["lookup xxxx","ok"]) 
        (runEval4c (eval4c two_vars_env watIsXxxx))
        

        
  main :: IO Counts
  main = runTestTT $ TestList [

    testEval4cLiteral,
    testEval4cVarXxxx,
    testEval4cVarUndefined,
    testEval4cWatIsXPlusY,
    testEval4cWatIsXPlusCrash
                               ]
