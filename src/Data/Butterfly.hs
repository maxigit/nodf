{-# LANGUAGE ExistentialQuantification, RankNTypes, DataKinds, KindSignatures, ScopedTypeVariables #-}
{-# LANGUAGE TupleSections, PatternGuards, FlexibleContexts #-}
module Data.NoDF.Butterfly where


import Prelude hiding(length)
import Data.Vector.Sized as S hiding(sequence, mapM_, map) -- (Vector)
import qualified Data.Vector.Mutable.Sized as MS -- (Vector)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector as Unsized
import Data.Finite
import GHC.TypeNats (Nat, KnownNat)
import qualified Data.Vector.Algorithms.Intro as A
import Data.Functor.Identity
import Control.Monad.ST
import Data.Coerce 


-- * Types

data VMapping f n g m  = 
     VMapping { ngroups :: Vector n (Finite g)  
               , ggroups :: Vector g (f (Finite m)) -- g -> ~m
               } 
     -- deriving (Show, Eq)
     

ordering :: forall a n r . (KnownNat n, Ord a) => Vector n a -> (forall n' . KnownNat n' => VMapping Identity n' n n' -> r ) -> r 
ordering v f = let
   ix = generate id :: Vector n (Finite n)
   in case GV.modify (A.sortBy (\i j -> compare (v `index` i) (v `index` j))) $ fromSized ix of
         SomeSized ( ix' :: KnownNat n' => Vector n' (Finite n)) -> let
              ggroups =  runST $ do
                  mv <- MS.unsafeNew
                  imapM_ (\i' i -> MS.write mv i (Identity i') ) ix'
                  freeze mv
              ngroups = ix' -- fmap Identity ix'
              in f $ VMapping ngroups ggroups
   
   {-
     v  : a d b c 
     ix : 1 2 3 4

     v  : a b c d 
     ix': 1 3 4 2
     i_1: 1 4 2 3
       
    -}

segmenting  :: forall a n r . (KnownNat n, Eq a) => Vector n a -> (forall g . KnownNat g  => VMapping Unsized.Vector n g n -> r) -> r
segmenting v f = let
   groupsWithValue = Unsized.groupBy (\a b -> snd a == snd b) (fromSized $ indexed v)
   groups = map (fmap fst) groupsWithValue  -- just keep the index
   in withSizedList groups $ \ggroups -> let
           ngroups = runST $ do
                 mv <- MS.unsafeNew
                 imapM_ (\gi (is :: Unsized.Vector (Finite n)) -> mapM_ (\i -> MS.write mv i gi ) is) ggroups
                 freeze mv
           
           in f $ VMapping ngroups ggroups
       
grouping :: forall a n r . (KnownNat n, Ord a) => Vector n a -> (forall g. KnownNat g => VMapping Unsized.Vector n g n -> r) -> r
grouping v f =
  ordering v $ \order -> segmenting (ngroups order @> v) $ \segments -> f ( compose order segments)

compose :: forall n n' g f . Functor f => VMapping Identity n' n n' -> VMapping f n' g n' -> VMapping f n g n
compose order segments = let
        ngroups' :: Vector n (Finite g)
        ngroups' = withVectorUnsafe coerce $ ngroups segments >@ ggroups order
        -- lift each group back to original indices
        ggroups' :: Vector g (f (Finite n))
        ggroups' =  ngroups order >@ ggroups segments

      in VMapping ngroups' ggroups'

(<@) :: Vector n a -> Vector n' (Finite n) -> Vector n' a
data_ <@ perm = backpermute data_ (fmap (fromIntegral . getFinite )  perm)
(@>) :: Vector n' (Finite n) -> Vector n a -> Vector n' a
(@>) = flip (<@)

(>@) :: Functor f => Vector n a -> Vector n' (f (Finite n)) -> Vector n' (f a)
data_ >@ perm = fmap (fmap (S.index data_ )) perm

(@<) :: Functor f => Vector n' (f (Finite n)) -> Vector n a -> Vector n' (f a)
(@<) = flip (>@)
main = do
   withSizedList ["a", "c", "m", "w", "c", "p" ] $ \v -> 
             ordering v $ \p -> do
                print v
                let 
                    sorted = ngroups p @> v
                print sorted
                print $ withVectorUnsafe coerce (ggroups p) @> sorted
                print " ====== SEGMENTING ===== "
                segmenting sorted $ \gr -> do
                  print $ ggroups gr @< sorted
                  print $ fmap runIdentity $ (ngroups gr @> (ggroups gr @< sorted )) >@ ggroups p
                print " ======= GROUPING ===== "
                grouping (fmap (`Prelude.elem` ["c"]) v) $ \gr -> do
                  print $ ggroups gr @< v
                  mapM_ print $ S.zip v (ngroups gr @> (ggroups gr @< v ))

