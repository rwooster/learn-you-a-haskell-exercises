import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Eq)

instance (Show a) => Show (List a) where
  show Empty = "{}"
  show (Value x Empty) = "{" ++ show x ++ "}"
  show (Value x xs) = "{" ++ show x ++ show' xs
    where show' (Value y Empty) = "," ++ show y ++ "}"
          show' (Value y ys) = "," ++ show y ++ show' ys

-- Make the list a Functor

instance Functor List where
  fmap f Empty = Empty 
  fmap f (Value x xs) = Value (f x) (fmap f xs)

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty b = b
combineLists a Empty = a
combineLists (Value a as) bs = Value a (combineLists as bs)

-- Make our list a Monoid

instance Semigroup (List a) where
  (<>) = combineLists

instance Monoid (List a) where
  mempty = Empty

-- Make our list an Applicative
instance Applicative List where
  pure f = Value f Empty
  Empty <*> as = Empty
  (Value f fs) <*> as = combineLists (fmap f as) (fs <*> as)

-- Make sure that the List obeys the laws for Applicative and Monoid

applicativeLaw1 = (pure f <*> x) == (fmap f x)
  where f = (+2)
        x = Value 3 (Value 2 (Value 1 Empty))

applicativeLaw2 = (pure id <*> v) == v
  where v = Value 3 (Value 2 (Value 1 Empty))

--applicativeLaw3 = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))
  --where u = Value 1 (Value 2 Empty)
        --v = Value 4 (Value 5 (Value 6 Empty))
        --w = Value 8 (Value 9 (Value 10 (Value 11 Empty)))

--applicateLaw 3 = (pure f <*> pure x) == (pure (f x))
  --where f = (+2)
        --x = Value 3 $ Value 2 $ Value 1 Empty

monoidLaw1 = (mempty <> x) == x
  where x = Value 3 $ Value 2 $ Value 1 Empty

monoidLaw2 = (x <> mempty) == x
  where x = Value 3 $ Value 2 $ Value 1 Empty

monoidLaw3 = ((x <> y) <> z) == (x <> (y <> z))
  where x = Value 3 $ Value 2 $ Value 1 Empty
        y = Value 4 $ Value 5 $ Value 6 Empty
        z = Value 7 $ Value 8 $ Value 9 Empty

-- Create some lists of numbers of different lengths such as:
a = Value 10 $ Value 20 Empty
b = Value 1 $ Value 2 $ Value 3 Empty
c = Value 15 $ Value 16 $ Value 17 $ Value 18 Empty
d = Value 10 Empty
e = Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)
a' = (+2) <$> a
d' = (+2) <$> d
e' = (+2) <$> e

-- Use <$> and <*> on the lists with a binary function

a'' = (+) <$> a <*> a
ae'' = (+) <$> a <*> e
ad'' = (+) <$> a <*> d
e'' = (+) <$> e <*> e

-- Create some lists of binary functions
math = Value (+) $ Value (*) Empty

-- Use <*> on the binary functions list and the number lists
ac_math = math <*> a <*> c

my_lift f a b = f <$> a <*> b
lists = [a, b]

fold_lift = foldr1 (my_lift (+)) lists
