
module Faust.ReaderSpec (main, spec) where

  import Data.Char
  import Test.Hspec
  import Faust.Reader

  main :: IO ()
  main = hspec spec

  legge :: Reader r r
  legge = Reader id
  leggeLength = fmap length legge

  spec :: Spec
  spec = do
    describe "a home-grown Reader" $ do
        it "can mimick ask" $ do
          runReader leggeLength "ciccio" `shouldBe` 6
