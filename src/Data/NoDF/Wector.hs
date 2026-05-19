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
import Data.Functor.Identity

import qualified Data.Foldable as F
import qualified Data.Foldable1 as F1
import qualified Data.List as List
import Data.Coerce (coerce, Coercible)
-- import qualified Data.Vector.Algorithms.Intro as Algo
import qualified Data.Vector.Algorithms.Tim as Algo
import Data.NoDF.Fold1

-- * Operations on permutation
--
infixl 5 @>, @>$, @>=, @>~, @=>
(@>) :: Functor f => f(Finite m) -> Vector m a -> f a
p @> v = fmap (index v) p

(@>$) :: Functor f => Vector n (f (Finite m)) -> Vector m a -> Vector n (f a)
p @>$ v = fmap (fmap (index v)) p

(@>=) :: Monad f => Vector n (f (Finite m)) -> Vector m (f a) -> Vector n (f a)
p @>= v = fmap ((index v) =<<) p

(@>~) :: (Traversable t , Monad f)  => Vector n (t (Finite m)) -> Vector m (f a ) -> Vector n (f (t a))
p @>~ v = sequence <$> p @>$ v


(@=>) :: F1.Foldable1 f1 => Vector n (f1 (Finite m)) -> Vector m a -> Vector n a
p @=> v = (head1 <$> p) @> v

coerceV :: Coercible a b =>  Vector n a -> Vector n b
coerceV = S.withVectorUnsafe coerce

-- | Apply a function to sub vector which keep their size identical
-- Allocate only one big vector
rmap :: KnownNat n => Vector grp (Unsized.Vector  (Finite n)) -> (forall s . Vector s a -> Vector s b) -> Maybe b -> Vector n a -> Vector n b
rmap grpv f b0m v =
   runST $ do
       mv <- case b0m of 
               Nothing ->  MS.unsafeNew
               Just b0 -> MS.replicate b0
       S.mapM (\g -> case g of
                         SomeSized gz -> do
                            S.imapM_ (\gi b -> -- index in the current group
                                                     MS.write mv (index gz gi) b
                                                  )
                                                  (f $ gz @> v)
              )
              grpv
       S.freeze mv
  
type Vector1  = Fold1 Unsized.Vector

-- | A "double" vector with a shared spine
data Wector n grp a =
            Wector { windex  :: Vector n (Finite grp)
                   , witems :: Vector grp a
                   }
     deriving (Show, Eq, Functor, Foldable, Traversable)
     
wbroadcast :: Wector n grp a -> Vector n a
wbroadcast w = windex w @> witems w

expandW :: Functor f => Wector n grp (f (Finite n)) -> Wector n grp (f (Finite grp))
expandW w = w { witems = witems w @>$ windex w }

wexpand :: Functor f => Wector n grp (f (Finite n)) -> Vector grp (f (Finite grp))
wexpand = witems . expandW

-- | when witems a <= witems ab
composeW :: Monad f => Wector a ab (f (Finite a)) -> Wector ab abcd (f (Finite ab)) -> Wector a abcd (f (Finite a))
composeW a ab = Wector ( windex a @> windex ab)
                      ( witems ab @>= witems a)
                      
-- | chaining selections
composing :: Monad f => ((Wector ab abcd (f (Finite ab))  -> r) -> r)
                     -> ((Wector a ab (f (Finite a)) -> r) -> r)
                     -> ((Wector a abcd (f (Finite a)) -> r ) -> r)
composing cab ca f = cab (\wab  -> ca (\wa -> f $ composeW wa wab ))


cab >.> ca = composing cab ca

-- | op can by @>= or @>~ or @>$
-- composeWith :: Monad f => Wector a ab (f (Finite a)) -> Wector ab abcd (f (Finite ab)) -> Wector a abcd (f (Finite a))
composeWith op a ab = Wector ( windex a @> windex ab)
                      ( witems ab `op` witems a)

inverseW :: Wector n grp (Identity (Finite n)) -> Wector grp n (Identity (Finite grp))
inverseW w = Wector ( coerceV $ witems w )
                    ( coerceV $ windex w)
                    

-- | op can be @> or @>$
broadcastWith :: Monad f => (Vector grp a -> Vector m (Finite grp) -> Vector g2 b) -> Wector n grp a -> Wector m grp (f (Finite g2)) -> f ( Wector n g2 b )
broadcastWith op a b = fmap go $ sequence (witems b) where
            go itemsb = Wector ( windex a @> itemsb)
                           ( witems a `op` windex b)

composeItems :: Functor f => (Wector m n (Identity (Finite m))) -> (Wector m grp (f (Finite m))) -> Wector n grp (f (Finite n))
composeItems a b = Wector (witems a @=> windex b)
                          ( witems b @>$ windex a)

ab >.< cb = \f -> (ab (\wab -> cb (\wcb -> f $ composeItems wab wcb)))

selecting ::  forall n r . KnownNat n => Vector n Bool -> (forall selected . KnownNat selected => Wector selected n (Maybe (Finite selected)) ->  r ) -> r
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

-- | specialized version of selecting which doesn't create an intermedaiat
filtering :: forall a n r . KnownNat n => (a -> Bool) -> Vector n a -> (forall filtered . KnownNat filtered => Wector filtered n (Maybe (Finite filtered)) -> r ) -> r
filtering keep v f = let
   selection = Unsized.filter (\fi -> keep (v `index` fi) )
                                                              $ fromSized 
                                                              $ S.generate id
   in case selection of
        SomeSized sel -> let 
           back = runST $ do
                        mv <- MS.replicate Nothing
                        S.imapM_ (\is i -> MS.write mv i (Just is)) sel
                        S.freeze mv
           in f $ Wector sel back

taking :: forall n a r . KnownNat n => Int -> Vector n a -> (forall taken . KnownNat taken => Wector taken n (Maybe (Finite taken)) -> r ) -> r
taking n v f = let 
   take = case n of 
               _ | n > 0 -> Unsized.take n
               _ | n == 0 -> id
               _ | n < 0 -> Unsized.drop (S.length v + n)
   selection = take $ fromSized 
                    $ S.generate id
   in case selection of
        SomeSized sel -> let 
           back = runST $ do
                        mv <- MS.replicate Nothing
                        S.imapM_ (\is i -> MS.write mv i (Just is)) sel
                        S.freeze mv
           in f $ Wector sel back

dropping :: forall n a r . KnownNat n => Int -> Vector n a -> (forall dropped . KnownNat dropped => Wector dropped n (Maybe (Finite dropped)) -> r) -> r
dropping n = selectingWithMaybe (Unsized.drop n)


droppingWhile :: forall n a r . KnownNat n => (a -> Bool) -> Vector n a -> (forall dropped . KnownNat dropped => Wector dropped n (Maybe (Finite dropped)) -> r) -> r
droppingWhile p v = selectingWithMaybe (Unsized.dropWhile pi) v where
    pi i = p (index v i )

takingWhile :: forall n a r . KnownNat n => (a -> Bool) -> Vector n a -> (forall taken . KnownNat taken => Wector taken n (Maybe (Finite taken)) -> r) -> r
takingWhile p v = selectingWithMaybe (Unsized.takeWhile pi) v where
    pi i = p (index v i )

-- | If subset should return Maybe insteaf of List
selectingWith  :: forall n a r . KnownNat n => (Unsized.Vector (Finite n) -> Unsized.Vector (Finite n)) -> Vector n a -> (forall sel . KnownNat sel => Wector sel n [Finite sel] -> r) -> r
selectingWith select v f = let
   selection = select $ fromSized 
                 $ S.generate id
   in case selection of
        SomeSized sel -> let 
           back = runST $ do
                        mv <- MS.replicate []
                        S.imapM_ (\is i -> MS.modify mv (is:) i) $ sel
                        S.freeze mv
           in f $ Wector sel back

-- | Like selectingWith but assume that element are not duplicated. Therefore, we can use a Maybe (present or not) instead of a list
selectingWithMaybe  :: forall n a r . KnownNat n => (Unsized.Vector (Finite n) -> Unsized.Vector (Finite n)) -> Vector n a -> (forall sel . KnownNat sel => Wector sel n (Maybe (Finite sel)) -> r) -> r
selectingWithMaybe select v f = let
   selection = select $ fromSized 
                 $ S.generate id
   in case selection of
        SomeSized sel -> let 
           back = runST $ do
                        mv <- MS.replicate Nothing
                        S.imapM_ (\is i -> MS.write mv i (Just is)) $ sel
                        S.freeze mv
           in f $ Wector sel back

ordering :: forall n a r . (KnownNat n, Ord a) => Vector n a -> (forall sorted . KnownNat sorted => Wector sorted n (Identity (Finite sorted )) -> r ) -> r
ordering v f = let
  ix = S.generate id :: Vector n (Finite n)
  in case Unsized.modify (Algo.sortBy (\i j -> compare (v `index` i) (v `index` j))) (fromSized ix) of
         SomeSized ( ix' :: KnownNat sorted => Vector sorted (Finite n)) -> let
              items =  runST $ do
                  mv <- MS.unsafeNew
                  S.imapM_ (\i' i -> MS.write mv i (Identity i') ) ix'
                  S.freeze mv
              in f $ Wector ix' items

orderingWith :: forall n b r . (KnownNat n) => (Finite n -> Finite n -> Ordering) -> (forall sorted . KnownNat sorted => Wector sorted n (Identity (Finite sorted )) -> r ) -> r
orderingWith cmp f = let
  ix = S.generate id :: Vector n (Finite n)
  in case Unsized.modify (Algo.sortBy cmp) (fromSized ix) of
         SomeSized ( ix' :: KnownNat sorted => Vector sorted (Finite n)) -> let
              items =  runST $ do
                  mv <- MS.unsafeNew
                  S.imapM_ (\i' i -> MS.write mv i (Identity i') ) ix'
                  S.freeze mv
              in f $ Wector ix' items
segmenting :: forall n a r . (KnownNat n, Eq a) => Vector n a -> (forall seg . KnownNat seg => Wector n seg (Vector1 (Finite n)) -> r ) -> r
segmenting v f = let
   groupsWithValue = Unsized.groupBy (\a b -> snd a == snd b) (fromSized $ S.indexed v)
   ugroups = map (UnsafeFold1 . fmap fst) groupsWithValue  -- just keep the index
   in withSizedList ugroups $ \(groups :: Vector seg (Vector1 (Finite n))) -> let
           gindex = runST $ do
                 mv <- MS.unsafeNew
                 S.imapM_ (\gi (is :: Vector1 (Finite n)) -> mapM_ (\i -> MS.write mv i gi ) is) groups
                 S.freeze mv
           
           in f $ Wector gindex groups
   
grouping :: forall n a r . (KnownNat n, Ord a) => Vector n a -> (forall grp . KnownNat grp => Wector n grp (Vector1 (Finite n)) -> r ) -> r
grouping v f = 
   -- TODO rewrite to allocate all slices as one vector then sliced
   -- segmenting do that, witems are slices of a main vector
   -- witems group should be the same if possible
   ordering v $ \order ->
            segmenting (windex order @> v) $ \seg ->
                       f (composeItems order seg)

-- | To be used to combine left or right joins
-- Technically we only use windex from grouping
-- so instead of JoinSpine a Wector n joined a would work to rejoin
-- However, we will lose the group information which can be used later
-- Also a Wector doesn't carry the fact that the vector has been sorted and is unique
data JoinSpine n joined a =
       JoinSpine { jsSpine :: AscU (Vector joined) a -- ^ sorted and unique vector
                 , jsGrouping :: Wector n joined (Vector1 (Finite n)) -- ^ the grouping
                 }
       deriving (Show, Eq)

makingJoinSpine :: (KnownNat n, Ord a) => Vector n a -> (forall joined . KnownNat joined => JoinSpine n joined a -> r) -> r
makingJoinSpine v f = 
   grouping v $ \grp -> let uniqV = UnsafeAscU $ witems grp @=> v
                        in f (JoinSpine uniqV grp)

mkSpine :: KnownNat joined => AscU (Vector joined) a -> JoinSpine joined joined a
mkSpine uniq@(AscU v) = JoinSpine uniq (Wector ixs groups)
   where ixs = S.generate id
         groups = fmap (UnsafeFold1 . Unsized.singleton) ixs

-- | left join to an existing join spine. 
rejoin  :: forall a n m joined . (Ord a, KnownNat m, KnownNat joined) => JoinSpine n joined a -> Vector m a -> Wector n joined (Unsized.Vector (Finite m))
rejoin spine v' = 
        grouping v' $ \grp' -> case grp' of 
          ( _ :: Wector n' grp' (Vector1 (Finite n'))) -> 
             -- get a unique represent for each group
             -- we assume that each groups are not empty
             let AscU uniqV = jsSpine spine
                 uniqV' = witems grp' @=> v' 
                 -- foreach i in the left group we try to find the corresponding value 
                 -- in the right group and collect the index in grp'
                 ix'ac :: Vector joined (Maybe (Finite grp'))
                 ix'ac = S.unfoldrN' (S.length' uniqV)
                                   (\(i,i'm) -> case i'm of 
                                   -- ^ ^^^
                                   -- |  |
                                   -- |  +--------- right cursor  Nothing if  last right value < left cussor (
                                   -- |                nothing to join anymore
                                   -- +------------ left cursor
                                                Nothing -> (Nothing, (i+1, Nothing))
                                                --          ^^^^^^^   ^^
                                                --            |        |
                                                --            |        +--- advance left cursor
                                                --            +------------ return value
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
                 expand :: Maybe (Finite grp') -> Unsized.Vector (Finite m)
                 expand gi'm = case gi'm of 
                                Nothing -> mempty
                                Just gi' -> unFold1 (witems grp' `S.index` gi')

             in Wector (windex $ jsGrouping spine) (expand <$> ix'ac) -- join

joining :: forall n m a r . (KnownNat n, KnownNat m, Ord a) => Vector n a -> Vector m a -> (forall joined . KnownNat joined => Wector n joined (Vector1 (Finite n)) -> Wector n joined (Unsized.Vector (Finite m)) -> r ) -> r
joining v v' f = makingJoinSpine v $ \spine -> f (jsGrouping spine) (rejoin spine v')

-- | Moving window
moving :: KnownNat n => Int -> Wector n n (Vector1 (Finite n))
moving size = let
    u = S.fromSized $ S.generate id
    windex = S.generate id
    witems = S.generate (\fi -> UnsafeFold1 $ Unsized.drop (fromIntegral fi + 1 - size) $ Unsized.take  (fromIntegral fi + 1) u)
    in Wector windex witems

main :: IO ()
main = do
  withSizedList [ ("Adam-Navy", "Adam", 2)
                , ("Adele-BLk", "Adele", 3)
                , ("Adam-Black", "Adam", 10)
                , ("Fiddle-Navy", "Fiddle", 64)
                , ("Fiddle-Black", "Fiddle", 17)
                , ("Fiddle-Blue", "Fiddle", 23)
                ] $ \sales -> do
     let (sku, style, qty) = S.unzip3 sales
     withSizedList [ ("Adam", "Plate")
                   , ("Fiddle", "Plate")
                   , ("Adele", "Cup")
                   ] $ \style'shape -> do
       let (style_shape, shape_shape) = S.unzip style'shape 
       
       joining style style_shape $ \by_style on_style_ -> do
         let Just on_style = traverse mkFold1 on_style_
         print " ------- BY SHAPE ------ "
         print on_style
         mapM_ print $ S.zip sku $  wbroadcast on_style @=> style'shape -- shape_shape
         print " ---- SUM by STYLE ---- "
         let qty_style = sum <$> witems by_style @>$ qty
         print qty_style
         let shape = wbroadcast on_style @=> shape_shape
         grouping shape $ \by_shape  ->  do
           print by_shape
           let qty_shape = sum <$> witems by_shape @>$ qty
           print qty_shape
           mapM_ print $ S.zip4 sku
                                (windex by_style @> qty_style)
                                (wbroadcast on_style @=> shape_shape )
                                (windex by_shape @> qty_shape)
                                
           print " ---- RUNNING ---- "
           print $ rmap (unFold1 <$> witems by_shape) (S.postscanl' (+) 0)  Nothing qty 
     print " --- Moving avegare -- "
     let ma = moving 2
     print $ witems ma @>$ qty
     print $ sum <$> witems ma @>$ qty
     print " ==== Moving sorted === "
     ordering sku  $ \by_sku ->  do
         -- let mo = composeWith _ (moving 2) by_sku
         let mo = composeItems by_sku (moving 2)
         mapM_ print $ S.zip sales $  wbroadcast mo @>$ qty


            

     
_main = do
   withSizedList [("a", 2), ("c", 1), ("m", 6), ("w", 0), ("c", 3) , ("p", 0) ] $ \v'q -> do
      let (v,q) = S.unzip v'q
      selecting (fmap (`elem` ["c", "w"]) v) $ \s1_ -> case F.toList <$> s1_ of
      -- selectingWith id v $ \s1_ -> case s1_ of
           (s1 :: Wector s1 n ([ (Finite s1)])) ->
                -- selectingWith (\u -> u <> Unsized.reverse u) (windex s1 @> v) $ \s2_ -> case s2_ of
                -- selectingWith (Unsized.take 1 . Unsized.drop 1) (windex s1 @> v) $ \s2_ -> case s2_ of
                selectingWith (Unsized.fromList . map snd
                                                . List.sort
                                                . map (\i -> (S.index v (S.index (windex s1) i), i))
                                                . Unsized.toList) (windex s1 @> v) $ \s2_ -> case s2_ of
                  (s2 :: Wector s2 s1 ([ (Finite s2) ])) -> do
                       print ("S1", s1)
                       let q1 = S.replicate $ S.sum $ windex s1 @> q :: Vector s1 Int
                       print ("S2", s2, windex s2 @> windex s1 @> v)
                       let q2 = S.iterateN (+1) 100 :: Vector s2 Int
                       print q2
                       
                       let s2' = composeWith (@>=) s2 $ F.toList <$> s1
                       print $ windex s2' @> q
                       print $ wexpand s2'  @>$ v'q
                       mapM_ print $ S.zip4 v q (witems s1 @>$ q1) (witems s2'  @>$ q2)
                       let s2m = traverse go s2'
                           go xs = case xs of 
                                      [x] -> Just (Just x)
                                      [] -> Just Nothing
                                      _ -> Nothing
                       case (s2m, traverse go s1) of
                          (Just s2M, Just s1M) -> mapM_ print $ S.zip3 v (witems s1M @>$ q1) (witems s2M  @>$ q2)
                          _ -> return ()
                           



        
