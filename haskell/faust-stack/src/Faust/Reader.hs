{-# LANGUAGE InstanceSigs #-}

module Faust.Reader where

  import Data.Char

  hurr = (+2)
  durr = (*10)

  monadic = do
    a <- hurr
    b <- durr
    return (a + b)

  capitalize = map toUpper

  composed = capitalize . reverse
  fmapped = fmap capitalize reverse

  apped = pure (,) <*> capitalize <*> reverse
  bound1 = do
    c <- capitalize
    r <- reverse
    return (c,r)
  bound2 = capitalize >>= (\c -> reverse >>= (\r -> return (c,r)))

  newtype Reader r a = Reader { runReader :: r -> a }
  instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap fab ra = Reader (\a -> fab (runReader ra a))
  instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader (\_ -> a)
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    Reader rfab <*> Reader ra = Reader (\r -> rfab r (ra r))

  newtype HumanName = HumanName String deriving (Eq, Show)
  newtype DogName = DogName String deriving (Eq, Show)
  newtype Address = Address String deriving (Eq, Show)

  data Person =
    Person {
      humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    } deriving (Eq, Show)

  data Dog =
    Dog {
      dogsName :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

  patty :: Person
  patty =
    Person (HumanName "Pattypatty")
           (DogName "Wafer")
           (Address "Bartolini")

  -- without Reader
  getPersonsDog :: Person -> Dog
  getPersonsDog p = Dog (dogName p) (address p)

  -- still without Reader, but remembering (->r)
  getPersonsDogR :: Person -> Dog
  getPersonsDogR = pure Dog <*> dogName <*> address
