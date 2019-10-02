{-
	TRANSFORMERZ - Monad Transformer in vanilla Haskell
  Nothing imported - just code
	Author: Marco Faustinelli (contacts@faustinelli.net)
	Web: http://faustinelli.net/
	     http://faustinelli.wordpress.com/
	Version: 1.0

	The MIT License - Copyright (c) 2015 Transformerz Project
-}

module Faust.FaustSpec (main, spec) where

  import Data.Maybe
  import qualified Data.Map as Map
  import Text.Show.Functions
  import Test.Hspec
  import Test.QuickCheck
  import Control.Exception (evaluate)
  import Faust.Faust

  import Control.Monad.Identity
  import Control.Monad.Trans.Maybe
  import Control.Monad.Fail

  main :: IO ()
  main = hspec spec

  -- Expressions for exercises
  -- var xxxx, yyyy
  watIsXxxx = Var "xxxx"
  watIsYyyy = Var "yyyy"
  twoVarsEnv = Map.insert "xxxx" (IntVal 123) (Map.insert "yyyy" (IntVal 234) Map.empty)
  xPlusY = Plus (Var "xxxx") (Var "yyyy")
  -- \x -> x
  lambdina = Lambda "x" (Var "x")
  -- \x -> \y -> x + y
  lambdona = Lambda "x" (Lambda "y" (Plus (Var "x") (Var "y")))
  -- 12 + (\x -> x)(4 + 2)
  sampletto = Plus (Lit 12) (App lambdina (Plus (Lit 4) (Lit 2))) -- IntVal 18
  -- (\x -> \y -> x + y) (4) (xxxx)
  samplone = App (App lambdona (Lit 4)) (Var "xxxx") -- IntVal (4 + xxxx)
----------------------------


  spec :: Spec
  spec = do
    describe "using monad transformers" $ do
      describe "eval0" $ do
        it "should lookup a var" $ do
          eval0 twoVarsEnv watIsXxxx `shouldBe` IntVal 123

        it "should sum two vars" $ do
          eval0 twoVarsEnv xPlusY `shouldBe` IntVal 357

        it "should make a simple application" $ do
          eval0 Map.empty sampletto `shouldBe` IntVal 18

        it "should make a complex application" $ do
          eval0 twoVarsEnv samplone `shouldBe` IntVal 127

        it "should make a partial application" $ do
          eval0 Map.empty (App lambdona (Lit 4)) `shouldBe`
            -- FunVal name exp env
            FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)])

      describe "eval1" $ do
        it "should lookup a var" $ do
          eval1 twoVarsEnv watIsXxxx `shouldBe` Identity (IntVal 123)

        it "should lookup another var" $ do
          eval1 twoVarsEnv (Var "yyyy") `shouldBe` Identity (IntVal 234)

        it "should sum two vars" $ do
          eval1 twoVarsEnv xPlusY `shouldBe` Identity (IntVal 357)

        it "should sum three vars" $ do
          eval1 twoVarsEnv (Plus (Var "xxxx") xPlusY) `shouldBe` Identity (IntVal 480)

        it "should make a simple application" $ do
          runEval1 (eval1 Map.empty sampletto) `shouldBe` IntVal 18

        it "should make a complex application" $ do
          runEval1 (eval1 twoVarsEnv samplone) `shouldBe` IntVal 127

        it "should make a partial application" $ do
          runEval1 (eval1 Map.empty (App lambdona (Lit 4))) `shouldBe`
            FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)])

          --       TestCase $ assertEqual "eval1 should make a partial application"
          --                              (FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)]))
          --                              (runEval1 $ eval1 Map.empty (App lambdona (Lit 4)))

      describe "eval2" $ do
        it "should make a simple application" $ do
          runEval2 (eval2 Map.empty sampletto) `shouldBe` Just (IntVal 18)

        it "should make a complex application" $ do
          runEval2 (eval2 twoVarsEnv samplone) `shouldBe` Just (IntVal 127)



       -- it "" $ do
       --    `shouldBe`
--
--   testEval1ComplexApp :: Test
--   testEval1ComplexApp =
--       TestCase $ assertEqual "eval1 should make a complex application"
--                              (IntVal 127) (runEval1 $ eval1 twoVarsEnv samplone)
--
--   testEval1CurriedApp :: Test
--   testEval1CurriedApp =
--       TestCase $ assertEqual "eval1 should make a partial application"
--                              (FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)]))
--                              (runEval1 $ eval1 Map.empty (App lambdona (Lit 4)))
-- -----------------------------------------
--   testEval2WatIsXxxx :: Test
--   testEval2WatIsXxxx =
--       TestCase $ assertEqual "eval2 should lookup a var"
--         (MaybeT (Identity (Just (IntVal 4)))) (eval2 (Map.fromList [("x",IntVal 4)]) (Var "x"))
--
--   testEval2WatIsXxxx2 :: Test
--   testEval2WatIsXxxx2 =
--       TestCase $ assertEqual "eval2 should lookup a var"
--         (Just (IntVal 4)) (runEval2 $ eval2 (Map.fromList [("x",IntVal 4)]) (Var "x"))
--
--   testEval2WatIsXPlusY :: Test
--   testEval2WatIsXPlusY =
--       TestCase $ assertEqual "eval2 should sum two vars"
--                              (Just (IntVal 357)) (runEval2 $ eval2 twoVarsEnv xPlusY)
--
--   testEval2WatIsXPlusCrash :: Test
--   testEval2WatIsXPlusCrash =
--       TestCase $ assertEqual "eval2 should fail summing stuff when one ain't IntVal"
--                              (Nothing) (runEval2 $ eval2 Map.empty (Plus (Lit 123) lambdina))
--
--   testEval2SimpleApp :: Test
--   testEval2SimpleApp =
--       TestCase $ assertEqual "eval2 should make a simple application"
--                              (Just (IntVal 18)) (runEval2 $ eval2 Map.empty sampletto)
--
--   testEval2ComplexApp :: Test
--   testEval2ComplexApp =
--       TestCase $ assertEqual "eval2 should make a complex application"
--                              (Just (IntVal 127)) (runEval2 $ eval2 twoVarsEnv samplone)
--
--   testEval2CurriedApp :: Test
--   testEval2CurriedApp =
--       TestCase $ assertEqual "eval2 should make a partial application"
--                              (Just (FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)])))
--                              (runEval2 $ eval2 Map.empty (App lambdona (Lit 4)))
--
--   testEval2CurriedApp2 :: Test
--   testEval2CurriedApp2 =
--       TestCase $ assertEqual "eval2 should spit Nothing in case of errors"
--                              (Nothing)
--                              (runEval2 $ eval2 Map.empty (App lambdona (Var "inesistente")))
{--}
-----------------------------------------
  --
  -- testEval3Lit123a :: Test
  -- testEval3Lit123a =
  --     TestCase $ assertEqual "eval3 should return a ET(ST) and update state"
  --       (Identity (Just (IntVal 123), 1))
  --       (unST (unET $ eval3 Map.empty (Lit 123)) 0)
  --
  -- testEval3Lit123b :: Test
  -- testEval3Lit123b =
  --     TestCase $ assertEqual "eval3 should evaluate a Literal and update state"
  --       (Just (IntVal 123), 1)
  --       (runEval3 0 (eval3 Map.empty (Lit 123)))
  --
  -- testEval3VarXxxx :: Test
  -- testEval3VarXxxx =
  --     TestCase $ assertEqual "eval3 should lookup a Var and update state"
  --       (Just (IntVal 123), 1)
  --       (runEval3 0 (eval3 twoVarsEnv watIsXxxx))
  --
  -- testEval3VarUndefined :: Test
  -- testEval3VarUndefined =
  --     TestCase $ assertEqual "eval3 should fail on non-existing vars and update state"
  --       (Nothing, 1)
  --       (runEval3 0 (eval3 twoVarsEnv (Var "zzzz")))
  --
  -- testEval3WatIsXPlusY :: Test
  -- testEval3WatIsXPlusY =
  --     TestCase $ assertEqual "eval3 should sum two vars"
  --                            (Just (IntVal 357), 3) (runEval3 0 $ eval3 twoVarsEnv xPlusY)
  --
  -- testEval3WatIsXPlusCrash :: Test
  -- testEval3WatIsXPlusCrash =
  --     TestCase $ assertEqual "eval3 should fail summing stuff when one ain't IntVal"
  --                            (Nothing, 3) (runEval3 0 $ eval3 Map.empty (Plus (Lit 123) lambdina))
  --
  -- testEval3SimpleApp :: Test
  -- testEval3SimpleApp =
  --     TestCase $ assertEqual "eval3 should make a simple application"
  --                            (Just (IntVal 18), 8) (runEval3 0 $ eval3 Map.empty sampletto)
  --
  -- testEval3ComplexApp :: Test
  -- testEval3ComplexApp =
  --     TestCase $ assertEqual "eval3 should make a complex application"
  --                            (Just (IntVal 127), 9) (runEval3 0 $ eval3 twoVarsEnv samplone)
  --
  -- testEval3CurriedApp :: Test
  -- testEval3CurriedApp =
  --     TestCase $ assertEqual "eval3 should make a partial application"
  --                            (Just (FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)])), 4)
  --                            (runEval3 0 $ eval3 Map.empty (App lambdona (Lit 4)))
  --
  -- testEval3CurriedApp2 :: Test
  -- testEval3CurriedApp2 =
  --     TestCase $ assertEqual "eval3 should spit Nothing in case of errors"
  --                            (Nothing, 3)
  --                            (runEval3 0 $ eval3 Map.empty (App lambdona (Var "inesistente")))

---------------------------------------

  -- main :: IO Counts
  -- main = runTestTT $ TestList [
  --   testEval0WatIsXxxx,
  --   testEval0WatIsXPlusY,
  --   testEval0SimpleApp,
  --   testEval0ComplexApp,
  --   testEval0CurriedApp,
  --   testEval1WatIsXxxx,
  --   testEval1WatIsXPlusY,
  --   testEval1SimpleApp,
  --   testEval1ComplexApp,
  --   testEval1CurriedApp,
  --   testEval2WatIsXxxx,
  --   testEval2WatIsXxxx2,
  --   testEval2WatIsXPlusY,
  --   testEval2WatIsXPlusCrash,
  --   testEval2SimpleApp,
  --   testEval2ComplexApp,
  --   testEval2CurriedApp,
  --   testEval2CurriedApp2--,
    -- testEval3Lit123a,
    -- testEval3Lit123b,
    -- testEval3VarXxxx,
    -- testEval3VarUndefined,
    -- testEval3WatIsXPlusY,
    -- testEval3WatIsXPlusCrash,
    -- testEval3SimpleApp,
    -- testEval3ComplexApp,
    -- testEval3CurriedApp,
    -- testEval3CurriedApp2
                               -- ]
