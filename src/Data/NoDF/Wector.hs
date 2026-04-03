{-# LANGUAGE DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes, DataKinds, KindSignatures, ScopedTypeVariables , TypeOperators #-}
module Data.NoDF.Wector where

import qualified Data.Vector.Sized as S
import Data.Vector.Sized(Vector, index, pattern SomeSized, fromSized, withSizedList, imap)
import qualified Data.Vector as Unsized
--import qualified Data.Vector.Sized.Unbox as UBS
import Data.Finite
import GHC.TypeNats --  (Nat, KnownNat)
import qualified Data.Vector.Mutable.Sized as MS -- (Vector)
import Control.Monad.ST


-- | A "double" vector with a shared spine
data Wector n grp a =
            Wector { windex  :: Vector n (Finite grp)
                   , wspine :: Vector grp a
                   }
     deriving (Show, Eq, Functor, Foldable, Traversable)
     
infixl 5 @>, <@, @$>, <$@
(@>) :: Vector n (Finite m) -> Vector m a -> Vector n a
p @> v = fmap (index v) p
(<@) :: Vector m a -> Vector n (Finite m) -> Vector n a
(<@) = flip (@>)

(@$>) :: Functor f => Vector n (f (Finite m)) -> Vector m a -> Vector n (f a)
p @$> v = fmap (fmap (index v)) p
(<$@) :: Functor f => Vector m a -> Vector n (f (Finite m)) -> Vector n (f a)
(<$@) = flip (@$>)


wbroadcast :: Wector n grp a -> Vector n a
wbroadcast w = windex w @> wspine w

wextra :: Functor f => Wector n grp (f (Finite n)) -> Vector grp (f (Finite grp))
wextra w = wspine w @$> windex w

infixl 5 @@<, @@$<, @@><, @@$><
(@@<) :: Wector n grp (Finite m) -> Vector m a -> Vector grp a
w @@< v = wspine w @> v

(@@$<) :: Functor f => Wector n grp (f (Finite m)) -> Vector m a -> Vector grp (f a)
w @@$< v = wspine w @$> v

(@@><) :: Wector n grp (Finite m) -> Vector m a -> Vector n a
w @@>< v = wbroadcast w @> v

(@@$><) :: Functor f => Wector n grp (f (Finite m)) -> Vector m a -> Vector n (f a)
w @@$>< v = wbroadcast w @$> v

(@@$<>) :: Functor f => Wector n grp (f (Finite n)) -> Vector grp a -> Vector grp (f a)
w @@$<> v = wextra w @$> v

nindex :: Functor f => Wector n grp (f a) -> Vector grp (f (Finite grp))
nindex w = imap (\i m -> fmap (const i) m) (wspine w )

compose wa wb = Wector (windex wa @> windex wb)
                       (wspine wa @> wspine wb)
composeF wa wb = Wector (windex wa @> windex wb)
                       (wspine wa @$> wspine wb)
composeI wa wb = Wector (windex wa <@ wspine wb)
                       (wspine wa @> windex wb)
composeI2 wa wb = Wector (windex wa <@ wspine wb)
                       (wspine wa @$> windex wb)

xx :: Monad f => Vector n (f (Finite m)) -> Vector m (f a) -> Vector n (f a)
xx va vb = fmap ((index (vb)) =<<) va

xxf f va vb = fmap ((f $ index (vb)))  va


selecting ::  forall n r . KnownNat n => Vector n Bool -> (forall s . KnownNat s => Wector s n (Maybe (Finite s)) ->  r ) -> r
selecting v f = let 
   selection = Unsized.filter (\fi -> v `index` fi )
                                                              $ fromSized 
                                                              $ S.generate id
   in case selection of
        SomeSized sel -> let 
           back = runST $ do
                        mv <- MS.replicate Nothing
                        S.imapM_ (\is i -> MS.write mv i (Just is)) sel
                        S.freeze mv
           in f $ Wector sel back

taking :: forall n a r . KnownNat n => Int -> Vector n a -> (forall s . KnownNat s => Wector s n (Maybe (Finite s)) -> r ) -> r
taking n v f = let 
   selection = Unsized.take n $ fromSized 
                              $ S.generate id
   in case selection of
        SomeSized sel -> let 
           back = runST $ do
                        mv <- MS.replicate Nothing
                        S.imapM_ (\is i -> MS.write mv i (Just is)) sel
                        S.freeze mv
           in f $ Wector sel back




main :: IO ()
main = do
   withSizedList [("a", 2), ("c", 1), ("m", 6), ("w", 0), ("c", 3) , ("p", 0) ] $ \v'q -> do
      let (v,q) = S.unzip v'q
      selecting (fmap (`elem` ["c", "w"]) v) $ \s1_ -> case s1_ of
           (s1 :: Wector s1 n (Maybe (Finite s1))) ->
                taking 1 (windex s1 @> v) $ \s2_ -> case s2_ of
                  (s2 :: Wector s2 s1 (Maybe (Finite s2))) -> do
                       print ("S1", s1)
                       print ("S2", s2)

                       let x = windex s2 @> windex s1 :: Vector s2 (Finite n)
                           y = xxf (=<<) (wspine s1) (wspine s2) :: Vector n (Maybe (Finite s2))
                           z = y @$> x :: Vector n (Maybe (Finite n))
                       print x
                       print y
                       print z
                       print $ z @$> v
        
