ppp = pure (+) <*> (+2) <*> (*10)

hurr = (+2)
durr = (*10)

ppp1 = pure (+) <*> hurr <*> durr

ppp2 = do
  a <- hurr
  b <- durr
  return (a + b)
  
import Data.Char
capitalize = map toUpper

composed = capitalize . reverse
fmapped = fmap capitalize reverse

apped = pure (,) <*> capitalize <*> reverse
bound = do
  c <- capitalize
  r <- reverse
  return (c,r)
bound2 = capitalize >>= (\c -> reverse >>= (\r -> return (c,r)))  

newtype Reader r a = Reader { runReader :: r -> a }
instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap fab ra = Reader (\a -> fab (runReader ra a))

legge :: Reader r r # it's ask!!
legge = Reader id
leggeLength = fmap length legge

# runReader leggeLength "ciccio" # 6

  
