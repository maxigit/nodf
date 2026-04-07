{-# LANGUAGE DeriveTraversable, PatternSynonyms, ViewPatterns #-}
module Data.NoDF.SUN where
import Data.Coerce


-- | Sorted and uniq
-- Not a functor instance because we can't
-- guarantie uniqueness or order
newtype SU f a = SortedUnique { unSU :: f a }
  deriving (Show, Eq, Ord, Foldable)
  
-- | Non null container
newtype NN f a = NonNull { unNN :: f a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
  
-- | Sorted Unique and Nonnull
newtype SUN f a = SUN { unSUN:: f a }
  deriving (Show, Eq, Ord, Foldable)
  
newtype SO f a = Sorted { unSO :: f a }
  deriving (Show, Eq, Ord, Foldable)
   
-- * Conversion

class Sorted s where
  sorted ::  s f a -> SO f a

instance Sorted SU where
  sorted = coerce
  
instance Sorted SUN where
  sorted = coerce
  
instance Sorted SO where
  sorted = id

class NonNull n where
   nonNull :: n f a -> NN f a

instance NonNull SUN where
   nonNull = coerce
   
instance NonNull NN where
   nonNull = id

class SortedUnique su where
   sortedUnique :: su f a -> SU f a

instance SortedUnique SU where
   sortedUnique = id
   
instance SortedUnique SUN where
   sortedUnique = coerce

pattern SU su <- (sortedUnique -> su)
pattern SO so <- (sorted -> so)
pattern NN nn <- (nonNull -> nn)
