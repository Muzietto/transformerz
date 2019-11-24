{-# LANGUAGE InstanceSigs, AllowAmbiguousTypes #-}

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
  instance Monad (Reader r) where
    return :: a -> Reader r a
    return = pure
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    Reader fra >>= faRrb =
      Reader (\r -> runReader (faRrb (fra r)) r)

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

  -- using plain record syntax
  getPersonsDog1 :: Person -> Dog
  getPersonsDog1 p = Dog (dogName p) (address p)

  -- using (->r) applicative and monad
  getPersonsDog2 :: Person -> Dog
  getPersonsDog2 = pure Dog <*> dogName <*> address
  getPersonsDog3 :: Person -> Dog
  getPersonsDog3 = do
    n <- dogName
    a <- address
    return $ Dog n a

  -- using LiftA2
  myLiftA2 :: Applicative fr =>
              (a -> b -> c) ->
              fr a -> fr b -> fr c
  myLiftA2 fabc fra frb =
    pure fabc <*> fra <*> frb

  getPersonsDogLift :: Person -> Dog
  getPersonsDogLift = myLiftA2 Dog dogName address

  -- eventually with Reader as applicative
  getPersonsDogA :: Reader Person Dog
  -- getPersonsDogA = pure (Reader Dog) <*> (Reader dogName) <*> (Reader address)
  getPersonsDogA = undefined

  -- eventually with Reader as monad
  getPersonsDogM :: Reader Person Dog
  getPersonsDogM = do
    p <- ask
    return $ Dog (dogName p) (address p)

  asks :: (r -> a) -> Reader r a
  asks = Reader

  ask :: Reader r r
  ask = Reader (\r -> r)

  -- implement http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html#g:4
