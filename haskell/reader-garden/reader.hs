{-# LANGUAGE InstanceSigs, StandaloneDeriving #-}

main :: IO ()
main = undefined

newtype Reader r a = Reader { runReader :: r -> a }

--runReader :: Reader r a -> r -> a

lettrice :: Reader String String
lettrice = Reader { runReader = \_ -> "ciao" }

-- Class Functor f where
--  fmap: (a -> b) -> f a -> f b

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
--fmap :: (a -> b) -> (r -> a) -> (r -> b)
  fmap fab rra = undefined
