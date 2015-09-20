{-
	TRANSFORMERZ - Monad Transformer in vanilla Haskell
  Nothing imported - just code
	Author: Marco Faustinelli (contacts@faustinelli.net)
	Web: http://faustinelli.net/
	     http://faustinelli.wordpress.com/
	Version: 1.0

	The MIT License - Copyright (c) 2015 Transformerz Project
-}

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


  testEval4dLiteral :: Test
  testEval4dLiteral = 
      TestCase $ assertEqual "eval4d should evaluate a silly literal while updating state"
                             ((Just (IntVal 18), ["literal"]), 1) (runEval4d 0 $ eval4d Map.empty (Lit 18))

  testEval4dSimpleApp :: Test
  testEval4dSimpleApp = 
      TestCase $ assertEqual "eval4d should make a simple application while updating state"
                             ((Just (IntVal 18), ["sum","literal","application","sum","literal","literal","sum ok","lambda","application ok","lookup x","lookup ok","sum ok"]), 8) (runEval4d 0 $ eval4d Map.empty sample)

  testEval4dComplexApp :: Test
  testEval4dComplexApp = 
      TestCase $ assertEqual "eval4d should make a complex application while updating state"
                             ((Just (IntVal 127), ["application","lookup xxxx","lookup ok","application","literal","lambda","application ok","lambda","application ok","sum","lookup x","lookup ok","lookup y","lookup ok","sum ok"]), 9) (runEval4d 0 $ eval4d two_vars_env samplone)

  testEval4dCurriedApp :: Test
  testEval4dCurriedApp = 
      TestCase $ assertEqual "eval4d should make a partial application while updating state"
                             ((Just (FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)])), ["application","literal","lambda","application ok","lambda"]), 4) 
                             (runEval4d 0 $ eval4d Map.empty (App lambdona (Lit 4)))

  testEval4dCurriedApp2 :: Test
  testEval4dCurriedApp2 = 
      TestCase $ assertEqual "eval4d should spit Nothing in case of errors while updating state"
                             ((Nothing, ["application","lookup inesistente","lookup ko"]), 2) 
                             (runEval4d 0 $ eval4d Map.empty (App lambdona (Var "inesistente")))

  testEval4dWatIsXPlusY :: Test
  testEval4dWatIsXPlusY = 
      TestCase $ assertEqual "eval4d should sum two vars while updating state"
                             ((Just (IntVal 357), ["sum",
                                                  "lookup xxxx",
                                                  "lookup ok",
                                                  "lookup yyyy",
                                                  "lookup ok",
                                                  "sum ok"
                                                  ]), 3) (runEval4d 0 $ eval4d two_vars_env xPlusY)

  testEval4dWatIsXPlusCrash :: Test
  testEval4dWatIsXPlusCrash = 
      TestCase $ assertEqual "eval4d should fail summing stuff when one ain't IntVal while updating state"
                             ((Nothing, ["sum",
                                       "literal",
                                       "lambda",
                                       "sum ko"
                             ]), 3) (runEval4d 0 $ eval4d Map.empty (Plus (Lit 123) lambdina))
        
  testEval4dVarUndefined :: Test
  testEval4dVarUndefined = 
      TestCase $ assertEqual "eval4d should fail on non-existing vars and write about it while updating state"
        ((Nothing, ["lookup zzzz","lookup ko"]), 1)
        (runEval4d 0 (eval4d two_vars_env (Var "zzzz")))

  testEval4dVarXxxx :: Test
  testEval4dVarXxxx = 
      TestCase $ assertEqual "eval4d should lookup a Var and write about it while updating state"
        ((Just (IntVal 123), ["lookup xxxx","lookup ok"]), 1) 
        (runEval4d 0 (eval4d two_vars_env watIsXxxx))
-----------

  testEval4cLiteral :: Test
  testEval4cLiteral = 
      TestCase $ assertEqual "eval4c should evaluate a silly literal"
                             (Just (IntVal 18), ["literal"]) (runEval4c $ eval4c Map.empty (Lit 18))
{--}
  testEval4cSimpleApp :: Test
  testEval4cSimpleApp = 
      TestCase $ assertEqual "eval4c should make a simple application"
                             (Just (IntVal 18), ["sum","literal","application","sum","literal","literal","sum ok","lambda","application ok","lookup x","lookup ok","sum ok"]) (runEval4c $ eval4c Map.empty sample)

  testEval4cComplexApp :: Test
  testEval4cComplexApp = 
      TestCase $ assertEqual "eval4c should make a complex application"
                             (Just (IntVal 127), ["application","lookup xxxx","lookup ok","application","literal","lambda","application ok","lambda","application ok","sum","lookup x","lookup ok","lookup y","lookup ok","sum ok"]) (runEval4c $ eval4c two_vars_env samplone)

  testEval4cCurriedApp :: Test
  testEval4cCurriedApp = 
      TestCase $ assertEqual "eval4c should make a partial application"
                             (Just (FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)])), ["application","literal","lambda","application ok","lambda"]) 
                             (runEval4c $ eval4c Map.empty (App lambdona (Lit 4)))

  testEval4cCurriedApp2 :: Test
  testEval4cCurriedApp2 = 
      TestCase $ assertEqual "eval4c should spit Nothing in case of errors"
                             (Nothing, ["application","lookup inesistente","lookup ko"]) 
                             (runEval4c $ eval4c Map.empty (App lambdona (Var "inesistente")))

  testEval4cWatIsXPlusY :: Test
  testEval4cWatIsXPlusY = 
      TestCase $ assertEqual "eval4c should sum two vars"
                             (Just (IntVal 357), ["sum",
                                                  "lookup xxxx",
                                                  "lookup ok",
                                                  "lookup yyyy",
                                                  "lookup ok",
                                                  "sum ok"
                                                  ]) (runEval4c $ eval4c two_vars_env xPlusY)

  testEval4cWatIsXPlusCrash :: Test
  testEval4cWatIsXPlusCrash = 
      TestCase $ assertEqual "eval4c should fail summing stuff when one ain't IntVal"
                             (Nothing, ["sum",
                                       "literal",
                                       "lambda",
                                       "sum ko"
                             ]) (runEval4c $ eval4c Map.empty (Plus (Lit 123) lambdina))
        
  testEval4cVarUndefined :: Test
  testEval4cVarUndefined = 
      TestCase $ assertEqual "eval4c should fail on non-existing vars and write about it"
        (Nothing, ["lookup zzzz","lookup ko"])
        (runEval4c (eval4c two_vars_env (Var "zzzz")))

  testEval4cVarXxxx :: Test
  testEval4cVarXxxx = 
      TestCase $ assertEqual "eval4c should lookup a Var and write about it"
        (Just (IntVal 123), ["lookup xxxx","lookup ok"]) 
        (runEval4c (eval4c two_vars_env watIsXxxx))
        

        
  main :: IO Counts
  main = runTestTT $ TestList [
                                testEval4cLiteral,
                                testEval4cVarXxxx,
                                testEval4cVarUndefined,
                                testEval4cWatIsXPlusY,
                                testEval4cWatIsXPlusCrash,
                                testEval4cSimpleApp,
                                testEval4cCurriedApp2,
                                testEval4cCurriedApp,
                                testEval4cComplexApp,
                                testEval4cSimpleApp,
                                
                                testEval4dLiteral,
                                testEval4dVarXxxx,
                                testEval4dVarUndefined,
                                testEval4dWatIsXPlusY,
                                testEval4dWatIsXPlusCrash,
                                testEval4dSimpleApp,
                                testEval4dCurriedApp2,
                                testEval4dCurriedApp,
                                testEval4dComplexApp,
                                testEval4dSimpleApp
                              ]
