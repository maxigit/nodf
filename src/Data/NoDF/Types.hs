{-# LANGUAGE ExistentialQuantification, RankNTypes, DataKinds, KindSignatures, ScopedTypeVariables #-}
{-# LANGUAGE TupleSections, PatternGuards, FlexibleContexts #-}
module Data.NoDF.Types
( PVector(..)
)
where


import Data.Vector.Sized as S -- (Vector)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector as Unsized
import Data.Finite
import GHC.TypeNats (Nat, KnownNat)
import qualified Data.Vector.Algorithms.Intro as A

-- | Permuted Vector
data PVector n a = forall ( m :: Nat)  .
     PVector { pvPermutations :: Vector n (Finite m)
             , pvData :: Vector m a
             }
     -- deriving (Show)
             
instance Show a => Show (PVector n a) where
    show (PVector perm data_ ) = show $ backpermute data_ (fmap (fromIntegral . getFinite )  perm) -- data_

-- | (data!) <$> perm  : Back permute operator
(<@) :: Vector n a -> Vector n' (Finite n) -> Vector n' a
data_ <@ perm = backpermute data_ (fmap (fromIntegral . getFinite )  perm)

(@>) = flip (<@)
(<$@) :: Functor f => Vector n a -> Vector n' (f (Finite n)) -> Vector n' (f a)
data_ <$@ perm = fmap (fmap (S.index data_ )) perm

(@$>) :: Functor f => Vector n' (f (Finite n)) -> Vector n a -> Vector n' (f a)
(@$>)= flip (<$@)

gmap :: (Unsized.Vector a -> b) -> Grouping g n n' -> Vector n a -> Vector g b
gmap f grp v = f <$> ( v <@ grPermutation grp <$@ grSlices grp )
-- | The result of a group operation.
-- This is not a group because there is actually no data associated
-- but only a way to group any Vector with the correct size
data Grouping g n n' = 
     Grouping { grPermutation :: Vector n' (Finite n) -- sorted order 
              -- , grEnds :: Vector g (Finite n') -- last representable of each group
              , grSlices :: Vector g (Unsized.Vector (Finite n')) -- last representable of each group
              , grDict :: Vector n (Finite g) -- map to group
              }
     deriving (Show, Ord, Eq)
     
(@@>) :: Grouping g n cn -> Vector n a -> Vector cn a
grp @@> v = grPermutation grp @> v

(@@<#) :: Grouping g n cn -> Vector cn a -> Vector g (Unsized.Vector a)
grp @@<# cv = grSlices grp @$> cv

(@@>#) :: Grouping g n cn -> Vector n a -> Vector g (Unsized.Vector a)
grp @@># v = grp @@<# (grp @@> v)

(<@@) = flip (@@>)
(#<@@) = flip (@@>#)
(#>@@) = flip (@@<#)
-- (>@@) = flip (@@<)

(@@#>) :: Grouping g n cn -> Vector g a -> Vector n a
grp @@#> gv = grDict grp @> gv

(<#@@) = flip (@@#>)
-- | Returns a vector so that the indices gives value in the ascending order
-- The length of the sorted vector is the same as the original vector
-- but we lose it in purpose to not mix sorted and non sorted vector.
-- To use the sorted vector it needs to be given a new n' which will be different
-- from a compiler point of view.
order :: forall n n' a . (KnownNat n , Ord a) => Vector n a -> Unsized.Vector (Finite n) 
order v = let
  indices = generate id :: Vector n (Finite n)
  in GV.modify (A.sortBy (\i j -> compare (v `index` i) (v `index` j))) $ fromSized indices
  
     
grouping :: (KnownNat n, Ord a) => Vector n a -> (forall n' g . (KnownNat n', KnownNat g) => Grouping g n n' -> r ) -> r
grouping v f =
  withSized (order v) $ \o ->
    let v_sorted = v <@ o

    in  withSizedList ( Unsized.groupBy (\(_,a) (_,b) -> a == b)
                                        (fromSized $ indexed v_sorted)
                      ) $ \( groups :: Vector g (Unsized.Vector (Finite n, a)) ) ->  let
         slices = fmap (fmap fst) groups
         -- create dictionr Finite n  -> Finite g
         nTog = GV.zip (fromSized o) $ mconcat $ toList $ imap (\gi vni -> fmap (const gi) vni)  slices
         -- sort by n
         nToGSorted = GV.modify (A.sortBy (\i j -> compare (fst i) (fst j))) nTog
         Just dict = toSized (fmap snd nToGSorted)

         in f (Grouping o slices dict)


joining :: (KnownNat n, KnownNat m, Ord a) => Vector n a -> Vector m a -> (forall g gm n' m' . (KnownNat g, KnownNat gm, KnownNat n', KnownNat m') => Grouping g n n' -> Grouping gm m m' ->  Vector n' (Maybe (Finite gm)) -> r) -> r
joining v v' f = 
   grouping v $ \gv -> 
      grouping v' $ \gv' ->
         let uniqV' = Unsized.head <$> gv' @@># v'
             uniqV = gv @@> v
             ix'ac = unfoldrN' (length' uniqV)
                               (\(i,i'm) -> case i'm of 
                                            Nothing -> (Nothing, (i+1, Nothing))
                                            Just i' -> let value = index uniqV i
                                                           findNext j' = let value' = index uniqV' j'
                                                                         in case compare value value' of 
                                                                              EQ -> Just (j', True)
                                                                              LT -> Just (j', False)
                                                                              GT -> next j' >>=  findNext 
                                                           next j' = if maxBound == j'
                                                                     then Nothing
                                                                     else Just (succ j' )
                                                       in case findNext i'  of
                                                            Nothing -> (Nothing, (i+1, Nothing)) -- nothing to find anymore
                                                            Just (fi, True) -> (Just fi, (i+1, Just fi)) -- TODO incremente
                                                            Just (fi, False) -> (Nothing, (i+1, Just fi))
                                                                     
                               )
                               (0, Just 0)

         in f gv gv' ix'ac
         
         
main :: IO ()
main = testJoin
testJoin =  do
  let color'qty'hue = [ ("fern", 1, "green")
                      , ("cyan", 20, "blue")
                      , ("darkblue", 13, "blue")
                      , ("lime", 40, "green")
                      , ("emerald", 5, "green")
                      , ("fuchsia", 16, "red")
                      ]

      hues'hue_code = [ ("green", "GRN")
                      , ("light", "LIGHT")
                      , ("blue", "BLU")
                      , ("red", "RED")
                      ]
  withSizedList color'qty'hue $ \(cqh :: Vector n (String, Double, String)) ->
   withSizedList hues'hue_code $ \(hhc :: Vector h (String, String)) -> do
     let (colorN, qtyN, hueN) = S.unzip3 cqh
         (hueH, codeH) = S.unzip hhc
     joining hueN hueH $ \grp grpH joinedHue ->  do
        print " === JOIN ======= "
        print joinedHue
        print " ===== SORTED ===== "
        print $ (grpH @@> codeH)
        print $ (grpH @@> hueH)
        print $ (grp @@># colorN)
        print $ (grp @@># hueN)

        print " ===== SORTED ===== "
        let Just codeN = toSized . fromSized =<< S.sequence (joinedHue @$> (Unsized.head <$> grpH @@># codeH))
        S.mapM_ print $ S.zip3 colorN qtyN codeN
        
        print " === BY CODE === "
        grouping codeN $ \byCode -> do
           print $ byCode @@># qtyN
           let sumByCode = Prelude.sum <$> byCode @@># qtyN
           print $ sumByCode <#@@ byCode
           S.mapM_ print $ S.zip4 (codeN ) colorN qtyN $ qtyN / sumByCode <#@@ byCode * 100
          
  


_basic = do
  let colors = ["red", "black", "blue", "green", "red", "blue"]
      qties =  [1, 2, 1, 10, 5, 1]
  withSizedList colors  $ \(v :: Vector n String) -> do
      let Just q = fromList qties
      let o = order v
      withSized o $ \o' -> do
                print $ S.zip (q <@ o') (v <@ o')
                
      grouping v $ \g -> do
           print " ======== GROUPING ======== "
           print g
           print $ v <@@ g
           print $ q <@@ g
           let groups =  v <@@ g <@ (S.map  Unsized.last $ grSlices g)
           print groups
           print " ======== SLICING ======== "
           let sums = S.map  (\(SomeSized p) -> S.sum (q <@ (grPermutation g <@ p)))
                                                         $ grSlices g
           let sums2 = q #<@@ g
           print sums
           print $ GV.sum <$> sums2 
           print $ S.zip sums groups
           print $ gmap GV.sum g q
           print $ gmap (\q'v -> case GV.unzip q'v of
                                  (q0, v0) -> (GV.last v0, GV.sum q0)
                        )
                        g
                        (S.zip q v)
           print " ============= BROADCAST ========= "
           print $ S.zip3 (q ) (groups <#@@ g) (sums <#@@ g)
           Prelude.mapM_ print ( S.zip v $ q / (sums <#@@ g) * 100)
