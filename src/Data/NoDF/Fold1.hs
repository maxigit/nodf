{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC  -Wno-missing-export-lists #-}
module Data.NoDF.Fold1
where


import qualified Data.Foldable as F
import qualified Data.Foldable1 as F1
import qualified Data.Vector.Sized as S
import qualified Data.Vector as Unsized

newtype Fold1 f a = UnsafeFold1 { unFold1 :: f a }
  deriving (Eq, Ord, Functor, Foldable, Traversable
           , Semigroup
           -- No Monoid instance because 'mempty' means empty 
           -- and fold1 means non empty
           , Num
           )
instance Show (f a) => Show (Fold1 f a) where
   show (Fold1 xs) = "F1:" <> show xs

instance Applicative f => Applicative (Fold1 f) where
  pure = Fold1 . pure
  (Fold1 f) <*> (Fold1 xs) = Fold1 (f <*> xs)
  
instance Monad f => Monad (Fold1 f) where
   (Fold1 xs) >>= f = let f0 = unFold1 . f
                      in UnsafeFold1 (xs >>= f0)

   
-- instance Num (f a) => I-

{-# COMPLETE Fold1 #-}
pattern Fold1 :: f a -> Fold1 f a
pattern Fold1 f = UnsafeFold1 f
  

instance F1.Foldable1 (Fold1 (S.Vector n)) where
 foldrMap1 f g (Fold1 s) = F1.foldrMap1 f g (Fold1 (S.fromSized s))

instance F1.Foldable1 (Fold1 Unsized.Vector) where
  foldrMap1 f g (Fold1 v) = let v0 = Unsized.last v
                                vs = Unsized.init v
                            in foldr g (f v0) vs

   
mkFold1 :: Foldable f => f a -> Maybe (Fold1 f a)  
mkFold1 xs = if null xs
             then Nothing
             else Just (UnsafeFold1 xs)
             
             

-- | Ascending 
newtype Asc f a = UnsafeAsc { unsafeAsc :: f a }
   deriving (Eq, Ord, Foldable)
   
instance Show (f a) => Show (Asc f a) where
   show (Asc f) = "Asc:" <> show f
   
{-# COMPLETE Asc #-}
pattern Asc :: f a -> Asc f a
pattern Asc f = UnsafeAsc f

mkAsc :: (Foldable f, Ord a) => f a -> Maybe (Asc f a)
mkAsc xs = fmap (const (UnsafeAsc xs)) 
         $ F.foldlM go Nothing xs
         where go prevW x = case prevW of
                                 Nothing -> Just (Just x)
                                 Just prev | prev <=  x -> Just (Just x)
                                 _ -> Nothing -- exit
              
           
-- | Strictly Ascending  or Ascending "Unique"
newtype AscU f a = UnsafeAscU { unsafeAscU :: f a }
   deriving (Eq, Ord, Foldable)
   
instance Show (f a) => Show (AscU f a) where
   show (AscU f) = "AscU:" <> show f

{-# COMPLETE AscU #-}
pattern AscU :: f a -> AscU f a
pattern AscU f = UnsafeAscU f

mkAscU :: (Foldable f, Ord a) => f a -> Maybe (AscU f a)
mkAscU xs = fmap (const (UnsafeAscU xs)) 
         $ F.foldlM go Nothing xs
         where go prevW x = case prevW of
                                 Nothing -> Just (Just x)
                                 Just prev | prev <  x -> Just (Just x)
                                 _ -> Nothing -- exit


{-# COMPLETE AscU1, Asc1 #-}
pattern AscU1  :: f a -> AscU (Fold1 f) a
pattern AscU1 f = AscU (Fold1 f)
pattern Asc1  :: f a -> Asc (Fold1 f) a
pattern Asc1 f = Asc (Fold1 f)

  
head1 :: F1.Foldable1 f => f a -> a
head1 = F1.head
