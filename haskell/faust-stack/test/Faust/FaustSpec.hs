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
  import Control.Monad.Reader
  import Control.Monad.Trans.Maybe
  import Control.Monad.Fail
  import System.IO.Unsafe
  import Data.Functor ((<&>))

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
        it "should evaluate a Lit" $ do
          eval0 Map.empty (Lit 123) `shouldBe` IntVal 123

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

      describe "eval0b (using Reader at the core)" $ do
        it "should evaluate a Lit" $ do
          runReader (eval0b (Lit 123)) twoVarsEnv  `shouldBe` (IntVal 123)

        it "should lookup var" $ do
          runReader (eval0b (Var "yyyy")) twoVarsEnv `shouldBe` (IntVal 234)

        it "should make a complex application" $ do
          runReader (eval0b samplone) twoVarsEnv `shouldBe` IntVal 127

        it "should make a partial application" $ do
          runReader (eval0b (App lambdona (Lit 4))) Map.empty `shouldBe`
            FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)])

      describe "eval0c :: Reader Env (Exp -> Value)" $ do
        it "should evaluate a Lit" $ do
          (runReader eval0c) twoVarsEnv (Lit 123) `shouldBe` (IntVal 123)

        it "should lookup var" $ do
          (runReader eval0c) twoVarsEnv (Var "yyyy") `shouldBe` (IntVal 234)

        it "should make a complex application" $ do
          (runReader eval0c) twoVarsEnv samplone `shouldBe` IntVal 127

        it "should make a partial application" $ do
          (runReader eval0c) Map.empty (App lambdona (Lit 4)) `shouldBe`
            FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)])

      describe "eval1 (using Identity at the core)" $ do
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

      describe "eval1b (using IO at the core)" $ do
        it "should lookup a var" $ do
          eval1b twoVarsEnv watIsXxxx `shouldReturn` IntVal 123

        it "should lookup another var" $ do
          eval1b twoVarsEnv (Var "yyyy") `shouldReturn` IntVal 234

        it "should sum two vars" $ do
          eval1b twoVarsEnv xPlusY `shouldReturn` IntVal 357

        it "should sum three vars" $ do
          eval1b twoVarsEnv (Plus (Var "xxxx") xPlusY) `shouldReturn` IntVal 480

        it "should make a simple application" $ do
          eval1b Map.empty sampletto `shouldReturn` IntVal 18

        it "should make a complex application" $ do
          eval1b twoVarsEnv samplone `shouldReturn` IntVal 127

        it "should make a partial application" $ do
          eval1b Map.empty (App lambdona (Lit 4)) `shouldReturn`
            FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)])

      -- newType MaybeT m a =  MaybeT { runMaybeT: MaybeT m a -> m (Maybe a) }
      -- runEval2 :: MaybeT Identity Value -> Maybe Value
      -- runEval2 = runIdentity . runMaybeT
      describe "eval2, using MaybeT Identity" $ do
        it "should make a simple application" $ do
          runEval2 (eval2 Map.empty sampletto) `shouldBe` Just (IntVal 18)

        it "should make a complex application" $ do
          runEval2 (eval2 twoVarsEnv samplone) `shouldBe` Just (IntVal 127)

        it "should make a complex application" $ do
          eval2 twoVarsEnv samplone `shouldBe`
  --        MaybeT $ Identity $ Just $ IntVal 127
            MaybeT (Identity (Just (IntVal 127)))

        it "should make a partial application" $
          eval2 Map.empty (App lambdona (Lit 4)) `shouldBe`
            MaybeT (Identity (Just (FunVal "y" (Plus (Var "x") (Var "y")) (Map.fromList [("x",IntVal 4)]))))

        it "should fail evaluating a Var not present in the env" $
          eval2 Map.empty (Var "pippo") `shouldBe` MaybeT (Identity (Nothing))

        it "should fail summing stuff when one ain't IntVal" $
          eval2 Map.empty (Plus (Lit 123) lambdina) `shouldBe` MaybeT (Identity (Nothing))

        it "should fail applying anything that ain't a FunVal" $
          eval2 Map.empty (App (Lit 123) (Lit 234)) `shouldBe` MaybeT (Identity (Nothing))

      describe "eval5, using ReaderT Identity" $ do
        it "should evaluate a Lit and return it as an Identity" $ do
          runReaderT (eval5 (Lit 123)) Map.empty `shouldBe` Identity (IntVal 123)

        it "should evaluate a Lit and return it as a Value thanks to runEval5" $ do
          runEval5 Map.empty (eval5 (Lit 123)) `shouldBe` (IntVal 123)

        -- it "should lookup another var" $ do
        --   eval5 twoVarsEnv (Var "yyyy") `shouldReturn` IntVal 234
        --
        -- it "should sum two vars" $ do
        --   eval5 twoVarsEnv xPlusY `shouldReturn` IntVal 357
