
module Faust.ReaderSpec (main, spec) where

  import Data.Char
  import Test.Hspec
  import Control.Applicative
  import Faust.Reader

  main :: IO ()
  main = hspec spec

  legge :: Reader r r
  legge = Reader id
  leggeLength = fmap length legge

  spec :: Spec
  spec = do
    describe "partially applied functions" $ do
      describe "are functors" $ do
        it "that can be fmapped" $ do
          fmap hurr durr 2 `shouldBe` 22
        it "also with more complex operations" $ do
          composed "ciao" `shouldBe` "OAIC"
          fmapped "ciao" `shouldBe` "OAIC"
      describe "are applicatives" $ do
        it "using pure and ap" $ do
          (pure (+) <*> hurr <*> durr) 2 `shouldBe` 24
        it "using the fake-fmap symbol" $ do
          ((+) <$> hurr <*> durr) 2 `shouldBe` 24
        it "that can be lifted" $ do
          (liftA2 (+) hurr durr) 2 `shouldBe` 24
        it "also with more complex operations" $ do
          apped "ciao" `shouldBe` ("CIAO","oaic")
      describe "are monads" $ do
        it "that can be bound on the fly" $ do
          (hurr >>= (\a -> durr >>= (\b -> return (a + b)))) 2 `shouldBe` 24
        it "that like sugar as well" $ do
          monadic 2 `shouldBe` 24
        it "also with more complex operations" $ do
          bound1 "ciao" `shouldBe` ("CIAO","oaic")
          bound2 "ciao" `shouldBe` ("CIAO","oaic")

    describe "persons and dog" $ do
      it "live together functionally happy" $ do
        dogsName (getPersonsDog patty) `shouldBe` DogName "Wafer"
      it "live together monadically happy" $ do
        dogsName (runReader getPersonsDogR patty) `shouldBe` DogName "Wafer"

    describe "a home-grown Reader" $ do
      it "can mimick ask" $ do
        runReader leggeLength "ciccio" `shouldBe` 6
      describe "is a functor" $ do
        it "that can be fmapped" $ do
          runReader (fmap capitalize (Reader reverse)) "ciao" `shouldBe` "OAIC"
      describe "is an applicative" $ do
        it "that can be apped" $ do
          runReader (pure (,) <*> (Reader capitalize) <*> (Reader reverse))  "ciao" `shouldBe` ("CIAO", "oaic")
