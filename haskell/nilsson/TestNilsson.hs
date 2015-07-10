
module TestNilsson where 

  import Data.Maybe
  import qualified Data.Map as Map
  import Text.Show.Functions
  import Test.HUnit
  import Nilsson

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

  testEval3WatIsXPlusY :: Test
  testEval3WatIsXPlusY = 
      TestCase $ assertEqual "eval3 should sum two vars"
                             (Just (IntVal 357), 3) (runEval3 0 $ eval3 two_vars_env xPlusY)

  testEval3WatIsXPlusCrash :: Test
  testEval3WatIsXPlusCrash = 
      TestCase $ assertEqual "eval3 should fail summing stuff when one ain't IntVal"
                             (Nothing, 3) (runEval3 0 $ eval3 Map.empty (Plus (Lit 123) lambdina))

  testEval3VarUndefined :: Test
  testEval3VarUndefined = 
      TestCase $ assertEqual "eval3 should fail on non-existing vars and update state"
        (Nothing, 1) 
        (runEval3 0 (eval3 two_vars_env (Var "zzzz")))

  testEval3VarXxxx :: Test
  testEval3VarXxxx = 
      TestCase $ assertEqual "eval3 should lookup a Var and update state"
        (Just (IntVal 123), 1) 
        (runEval3 0 (eval3 two_vars_env watIsXxxx))

---------------------------------------

  testEval3Lit123a :: Test
  testEval3Lit123a = 
      TestCase $ assertEqual "eval3 should return a ET(ST) and update state"
        (I (Just (IntVal 123), 1)) 
        (unST (unET $ eval3 Map.empty (Lit 123)) 0)

  testEval3Lit123b :: Test
  testEval3Lit123b = 
      TestCase $ assertEqual "eval3 should evaluate a Literal and update state"
        (Just (IntVal 123), 1) 
        (runEval3 0 (eval3 Map.empty (Lit 123)))

---------------------------------------
  testEval2WatIsXxxx :: Test
  testEval2WatIsXxxx = 
      TestCase $ assertEqual "eval2 should lookup a var"
        (ET (I (Just (IntVal 4)))) (eval2 (Map.fromList [("x",IntVal 4)]) (Var "x"))

  testEval2WatIsXxxx2 :: Test
  testEval2WatIsXxxx2 = 
      TestCase $ assertEqual "eval2 should lookup a var"
        (Just (IntVal 4)) (runEval2 $ eval2 (Map.fromList [("x",IntVal 4)]) (Var "x"))

  testEval2WatIsXPlusY :: Test
  testEval2WatIsXPlusY = 
      TestCase $ assertEqual "eval2 should sum two vars"
                             (Just (IntVal 357)) (runEval2 $ eval2 two_vars_env xPlusY)

  testEval2WatIsXPlusCrash :: Test
  testEval2WatIsXPlusCrash = 
      TestCase $ assertEqual "eval2 should fail summing stuff when one ain't IntVal"
                             (Nothing) (runEval2 $ eval2 Map.empty (Plus (Lit 123) lambdina))

  testEval2SimpleApp :: Test
  testEval2SimpleApp = 
      TestCase $ assertEqual "eval2 should make a simple application"
                             (Just (IntVal 18)) (runEval2 $ eval2 Map.empty sample)

  testEval2ComplexApp :: Test
  testEval2ComplexApp = 
      TestCase $ assertEqual "eval2 should make a complex application"
                             (Just (IntVal 127)) (runEval2 $ eval2 two_vars_env samplone)

  testEval2CurriedApp :: Test
  testEval2CurriedApp = 
      TestCase $ assertEqual "eval2 should make a partial application"
                             (Just (FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)]))) 
                             (runEval2 $ eval2 Map.empty (App lambdona (Lit 4)))

  testEval2CurriedApp2 :: Test
  testEval2CurriedApp2 = 
      TestCase $ assertEqual "eval2 should spit Nothing in case of errore"
                             (Nothing) 
                             (runEval2 $ eval2 Map.empty (App lambdona (Var "inesistente")))
{--}
-----------------------------------------

  testEval1WatIsXxxx :: Test
  testEval1WatIsXxxx = 
      TestCase $ assertEqual "eval1 should lookup a var"
                             (I (IntVal 123)) (eval1 two_vars_env watIsXxxx)

  testEval1WatIsXPlusY :: Test
  testEval1WatIsXPlusY = 
      TestCase $ assertEqual "eval1 should sum two vars"
                             (IntVal 357) (unI $ eval1 two_vars_env xPlusY)

  testEval1SimpleApp :: Test
  testEval1SimpleApp = 
      TestCase $ assertEqual "eval1 should make a simple application"
                             (IntVal 18) (runEval1 $ eval1 Map.empty sample)

  testEval1ComplexApp :: Test
  testEval1ComplexApp = 
      TestCase $ assertEqual "eval1 should make a complex application"
                             (IntVal 127) (runEval1 $ eval1 two_vars_env samplone)

  testEval1CurriedApp :: Test
  testEval1CurriedApp = 
      TestCase $ assertEqual "eval1 should make a partial application"
                             (FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)])) 
                             (runEval1 $ eval1 Map.empty (App lambdona (Lit 4)))
-----------------------------------------

  testEval0WatIsXxxx :: Test
  testEval0WatIsXxxx = 
      TestCase $ assertEqual "eval0 should lookup a var"
                             (IntVal 123) (eval0 two_vars_env watIsXxxx)

  testEval0WatIsXPlusY :: Test
  testEval0WatIsXPlusY = 
      TestCase $ assertEqual "eval0 should sum two vars"
                             (IntVal 357) (eval0 two_vars_env xPlusY)

  testEval0SimpleApp :: Test
  testEval0SimpleApp = 
      TestCase $ assertEqual "eval0 should make a simple application"
                             (IntVal 18) (eval0 Map.empty sample)

  testEval0ComplexApp :: Test
  testEval0ComplexApp = 
      TestCase $ assertEqual "eval0 should make a complex application"
                             (IntVal 127) (eval0 two_vars_env samplone)

  testEval0CurriedApp :: Test
  testEval0CurriedApp = 
      TestCase $ assertEqual "eval0 should make a partial application"
                             (FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)])) 
                             (eval0 Map.empty (App lambdona (Lit 4)))

  main :: IO Counts
  main = runTestTT $ TestList [
    testEval0WatIsXxxx,
    testEval0WatIsXPlusY,
    testEval0SimpleApp,
    testEval0ComplexApp,
    testEval0CurriedApp,
    testEval1WatIsXxxx,
    testEval1WatIsXPlusY,
    testEval1SimpleApp,
    testEval1ComplexApp,
    testEval1CurriedApp,
    testEval2WatIsXxxx,
    testEval2WatIsXxxx2,
    testEval2WatIsXPlusY,
    testEval2WatIsXPlusCrash,
    testEval2SimpleApp,
    testEval2ComplexApp,
    testEval2CurriedApp,
    testEval2CurriedApp2,
    testEval3Lit123a,
    testEval3Lit123b,
    testEval3VarXxxx,
    testEval3VarUndefined,
    testEval3WatIsXPlusY,
    testEval3WatIsXPlusCrash
                               ]
