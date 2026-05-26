{-# LANGUAGE DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes, DataKinds, KindSignatures, TypeAbstractions , TypeOperators #-}
module Data.NoDF.Wector 
( module Data.NoDF.Wector
)
where

import qualified Data.Vector.Sized as S
import Data.Vector.Sized(Vector, index, pattern SomeSized, fromSized, withSizedList)
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
       S.mapM_ (\g -> case g of
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
data Wector x v a =
            Wector { windex  :: Vector x (Finite v)
                   , walues :: Vector v a
                   }
     deriving (Show, Eq, Functor, Foldable, Traversable)

type WectorFF f x v y = Wector x v (f (Finite y))
     
-- | Self Wector with Existential index
data Wix f v = forall x . KnownNat x => Wix (WectorFF f x v x)
-- | Self Wector with Existential values
data Wal f x = forall v . KnownNat v => Wal (WectorFF f x v x)

pattern Wixor x v = Wix (Wector x v)
pattern Walor x v = Wal (Wector x v)

wbroadcast :: Wector x v a -> Vector x a
wbroadcast (Wector xV va) = xV @> va

expandW :: Functor f => WectorFF f x v x -> WectorFF f x v v
expandW w@(Wector xV vXX) = w { walues = vXX @>$ xV } -- w { walues = walues xVvXX @>$ windex xVvXX }
--                                    vXX            xV     vVV :: nv (

wexpand :: Functor f => WectorFF f nx nv nx -> Vector nv (f (Finite nv))
wexpand = walues . expandW

{-
composeW :: (Monad f, av__m ~ bx__m)
         => Wector ax__n av__m       (f (Finite ax__n))
         -> Wector       bx__m bv__o (f (Finite bx__m))
         -> Wector ax__n       bv__o (f (Finite ax__n))
-}
composeW :: (Monad f, av__m ~ bx__m) =>  WectorFF f ax__n av__m ax__n -> WectorFF f bx__m bv__o bx__m -> WectorFF f ax__n bv__o ax__n
composeW (Wector nM mNN) (Wector mO oMM) = 
         Wector (nM @> mO)
                (oMM @>= mNN)
-- | chaining selections
{-
composing, (>.>) :: (Monad f, av__m ~ bx__m)
                 => ((Wector ax__n av__m       (f (Finite ax__n)) -> r ) -> r)
                 -> ((Wector       bx__m bv__o (f (Finite bx__m)) -> r ) -> r)
                 -> ((Wector ax__n       bv__o (f (Finite ax__n)) -> r ) -> r)
-}
composing, (>.>) :: (Monad f, av__m ~ bx__m) => ((WectorFF f ax__n av__m ax__n -> r ) -> r) -> ((WectorFF f bx__m bv__o bx__m -> r ) -> r) -> ((WectorFF f ax__n bv__o ax__n -> r ) -> r)
composing nMmNN_f mOoMM_f f = nMmNN_f (\nMmNN  -> mOoMM_f (\mOoMM -> f $ composeW nMmNN mOoMM ))
cab >.> ca = composing cab ca

compX :: Monad f => Wix f v -> WectorFF f v u v -> Wix f u 
compX (Wix a) b = Wix $ composeW a b
compL :: Monad f => WectorFF f x v x -> Wal f v -> Wal f x
compL a (Wal b) = Wal $ composeW a b
-- | op can by @>= or @>~ or @>$
-- composeWith :: Monad f => WectorFF f a ab a -> WectorFF f ab abcd ab -> WectorFF f a abcd a
{- composeWith :: (Monad f, av__m ~ bx__m)
 -             => Wector ax__n av__m      (f (Finite ax__n))
 -             -> Wector       bx__m bv__o (f (Finite av__m))
 -             -> Wector ax__n       bv__o (f (Finite ax__n))
 -}
-- composeWith :: (Monad f, av__m ~ bx__m) =>  _ -> WectorFF f ax__n av__m ax__n -> WectorFF f bx__m bv__o bx__m -> WectorFF f ax__n bv__o ax__n
composeWith :: (av__m ~ bx__m) => (Vector bv__o b -> Vector av__m  a -> Vector bv__o c ) -> Wector ax__n av__m a -> Wector bx__m bv__o b -> Wector ax__n bv__o c
composeWith op (Wector nM mNN) (Wector mO oMM) = 
         Wector (nM @> mO)
                (oMM `op` mNN)

inverseW :: WectorFF Identity x v x -> WectorFF Identity v x v
inverseW w = Wector ( coerceV $ walues w )
                    ( coerceV $ windex w)
                    

-- | op can be @> or @>$
broadcastWith :: Monad f => (Vector av__g a -> Vector bx__m (Finite av__g) -> Vector rx__g2 b) -> Wector ax__n av__g a -> WectorFF f bx__m av__g rx__g2 -> f ( Wector ax__n rx__g2 b )
{-
broadcastWith :: (Monad f, bv__g ~ av__g )
              => (   Vector             av__g         a -> Vector bx__m (Finite av__g) -> Vector rx__g2 b)
              ->     Wector ax__n       av__g         a
              ->     Wector       bx__m bv__g         (f (Finite rx__g2))
              -> f ( Wector ax__n             rx__g2  b )
 -}
broadcastWith op _a@(Wector nG ga)  _b@(Wector mG gG2s)  = fmap go $ sequence gG2s where
            go gG2 = Wector ( nG @> gG2)
                           ( ga `op` mG)

{-
crossCompose :: Functor f
              => Wector ax__n av__m         (Identity (Finite ax__n))
              -> Wector ax__n       bv__grp (f (Finite ax__n))
              -> Wector       av__m bv__grp (f (Finite av__m))
-}
crossCompose :: Functor f => WectorFF Identity ax__n av__m ax__n -> WectorFF f ax__n bv__grp ax__n -> WectorFF f av__m bv__grp av__m
crossCompose (Wector ax_nM av_mNN) (Wector nG gNN) = 
    Wector (av_mNN @=> nG)
           (gNN @>$ ax_nM)

{-
crossCompose :: Functor f
              => Wector ax__n av__m         (Identity (Finite ax__n))
              -> Wector ax__n       bv__grp (f (Finite ax__n))
              -> Wector       av__m bv__grp (f (Finite av__m))
-}

crossing, (>.<) :: Functor f
              => ((Wector ax__n av__m         (Identity (Finite ax__n)) -> r) -> r )
              -> ((Wector ax__n       bv__grp (f (Finite ax__n)) -> r) -> r )
              -> ((Wector       av__m bv__grp (f (Finite av__m)) -> r) -> r )
-- crossing ab cb = \f -> (ab (\wab -> cb (\wcb -> f $ crossCompose wab wcb)))
crossing nMmNN_f nGgNN_f f = nMmNN_f (\nMmNN -> nGgNN_f (\nGgNN -> f $ crossCompose nMmNN nGgNN))
ab >.< cb = crossing ab cb

selectX :: KnownNat v => Vector v Bool -> Wix Maybe v
selectX v = let 
   selection = Unsized.filter (\fi -> v `index` fi )
                            $ fromSized 
                            $ S.generate id
   in case selection of
        SomeSized sel -> let 
           back = runST $ do
                        mv <- MS.replicate Nothing
                        S.imapM_ (\is i -> MS.write mv i (Just is)) sel
                        S.freeze mv
           in Wix $ Wector sel back

-- | specialized version of selecting which doesn't create an intermedaiat
filterX :: forall a v . KnownNat v => (a -> Bool) -> Vector v a -> Wix Maybe v 
filterX keep v = let
   selection = Unsized.filter (\fi -> keep (v `index` fi) )
                                                              $ fromSized 
                                                              $ S.generate id
   in case selection of
        SomeSized sel -> let 
           back = runST $ do
                        mv <- MS.replicate Nothing
                        S.imapM_ (\is i -> MS.write mv i (Just is)) sel
                        S.freeze mv
           in Wix $ Wector sel back

takeX :: KnownNat v => Int -> Vector v a -> Wix Maybe v
takeX n v = let 
   take_ = case n of 
               _ | n > 0 -> Unsized.take n
               _ | n == 0 -> id
               _ {- | n < 0 -} -> Unsized.drop (S.length v + n)
   selection = take_ $ fromSized 
                    $ S.generate id
   in case selection of
        SomeSized sel -> let 
           back = runST $ do
                        mv <- MS.replicate Nothing
                        S.imapM_ (\is i -> MS.write mv i (Just is)) sel
                        S.freeze mv
           in Wix $ Wector sel back

dropX :: KnownNat v => Int -> Vector v a -> Wix Maybe v
dropX n = selectWithMaybeX (Unsized.drop n)


-- droppingWhile :: forall v__n a r . KnownNat v__n => (a -> Bool) -> Vector v__n a -> (forall x__dropped . KnownNat x__dropped => WectorFF Maybe x__dropped v__n x__dropped -> r) -> r
dropWhileX :: KnownNat v => (a -> Bool) -> Vector v a -> Wix Maybe v
dropWhileX p v = selectWithMaybeX (Unsized.dropWhile pi_) v where
    pi_ i = p (index v i )

takeWhileX :: KnownNat v => (a -> Bool) -> Vector v a -> Wix Maybe v
takeWhileX p v = selectWithMaybeX (Unsized.takeWhile pi_) v where
    pi_ i = p (index v i )

-- | If subset should return Maybe insteaf of List
-- selectingWith  :: forall v__n a r . KnownNat v__n => (Unsized.Vector (Finite v__n) -> Unsized.Vector (Finite v__n)) -> Vector v__n a -> (forall x__selected . KnownNat x__selected => WectorFF []  x__selected v__n x__selected -> r) -> r
selectWithX  :: KnownNat v => (Unsized.Vector (Finite v) -> Unsized.Vector (Finite v)) -> Vector v a -> Wix [] v
selectWithX select _proxy_v = let
   selection = select $ fromSized 
                 $ S.generate id
   in case selection of
        SomeSized sel -> let 
           back = runST $ do
                        mv <- MS.replicate []
                        S.imapM_ (\is i -> MS.modify mv (is:) i) $ sel
                        S.freeze mv
           in Wix $ Wector sel back

-- | Like selectingWith but assume that element are not duplicated. Therefore, we can use a Maybe (present or not) instead of a list
-- selectingWithMaybe  :: forall v__n a r . KnownNat v__n => (Unsized.Vector (Finite v__n) -> Unsized.Vector (Finite v__n)) -> Vector v__n a -> (forall x__selected . KnownNat x__selected => WectorFF Maybe x__selected v__n x__selected -> r) -> r
selectWithMaybeX :: KnownNat v => (Unsized.Vector (Finite v) -> Unsized.Vector (Finite v)) -> Vector v a -> Wix Maybe v 
selectWithMaybeX select _proxy_v = let
   selection = select $ fromSized 
                 $ S.generate id
   in case selection of
        SomeSized sel -> let 
           back = runST $ do
                        mv <- MS.replicate Nothing
                        S.imapM_ (\is i -> MS.write mv i (Just is)) $ sel
                        S.freeze mv
           in Wix $ Wector sel back

orderX :: (KnownNat v, Ord a) => Vector v a -> Wix Identity v
orderX @v v = let
  ix = S.generate id :: Vector v (Finite v)
  in case Unsized.modify (Algo.sortBy (\i j -> compare (v `index` i) (v `index` j))) (fromSized ix) of
         SomeSized ( ix' :: KnownNat x__sorted => Vector x__sorted (Finite v)) -> let
              items =  runST $ do
                  mv <- MS.unsafeNew
                  S.imapM_ (\i' i -> MS.write mv i (Identity i') ) ix'
                  S.freeze mv
              in Wix $ Wector ix' items

orderV :: (KnownNat x, Ord a) => Vector x a -> Wal Identity x
orderV v = case orderX v of
             Wix w -> Wal $ inverseW w

-- orderingWith :: forall v__n b r . (KnownNat v__n) => (Finite v__n -> Finite v__n -> Ordering) -> (forall x__sorted . KnownNat x__sorted => WectorFF Identity x__sorted v__n x__sorted  -> r ) -> r
orderWithX :: KnownNat v => (Finite v -> Finite v -> Ordering) -> Wix Identity v
orderWithX @v cmp = let
  ix = S.generate id :: Vector v (Finite v)
  in case Unsized.modify (Algo.sortBy cmp) (fromSized ix) of
         SomeSized ( ix' :: KnownNat x__sorted => Vector x__sorted (Finite v__n)) -> let
              items =  runST $ do
                  mv <- MS.unsafeNew
                  S.imapM_ (\i' i -> MS.write mv i (Identity i') ) ix'
                  S.freeze mv
              in Wix $ Wector ix' items

segmentV :: (Eq a, KnownNat x) => Vector x a -> Wal Vector1 x
segmentV v = let
   groupsWithValue = Unsized.groupBy (\a b -> snd a == snd b) (fromSized $ S.indexed v)
   ugroups = map (UnsafeFold1 . fmap fst) groupsWithValue  -- just keep the index
   in withSizedList ugroups $ \(groups :: Vector seg (Vector1 (Finite n))) -> let
           gindex = runST $ do
                 mv <- MS.unsafeNew
                 S.imapM_ (\gi (is :: Vector1 (Finite n)) -> mapM_ (\i -> MS.write mv i gi ) is) groups
                 S.freeze mv
           
           in Wal $ Wector gindex groups
   
-- grouping :: forall x__n a r . (KnownNat x__n, Ord a) => Vector x__n a -> (forall v__grp . KnownNat v__grp => WectorFF Vector1 x__n v__grp x__n -> r ) -> r
-- TODO rewrite to allocate all slices as one vector then sliced
-- segmenting do that, walues are slices of a main vector
-- walues group should be the same if possible
groupV :: (KnownNat x, Ord a) => Vector x a -> Wal Vector1 x 
groupV v | Wix xOoX <- orderX v
         , Wal oGgO <- segmentV (windex xOoX @> v)
         = Wal $ crossCompose xOoX oGgO

-- | To be used to combine left or right joins
-- Technically we only use windex from grouping
-- so instead of JoinSpine a Wector n joined a would work to rejoin
-- However, we will lose the group information which can be used later
-- Also a Wector doesn't carry the fact that the vector has been sorted and is unique
data JoinSpine x__n v__joined a =
       JoinSpine { jsSpine :: AscU (Vector v__joined) a -- ^ sorted and unique vector
                 , jsGrouping :: WectorFF Vector1 x__n v__joined x__n -- ^ the grouping
                 }
       deriving (Show, Eq)

data JoinSpineV x a = forall v . KnownNat v => JoinSpineV (JoinSpine x v a)

makeJoinSpineV :: (KnownNat x, Ord a) => Vector x a -> JoinSpineV x a
makeJoinSpineV v | Wal grp <- groupV v
                 , let  uniqV = UnsafeAscU $ walues grp @=> v
                 = JoinSpineV $ JoinSpine uniqV grp

mkSpine :: KnownNat xv_joined => AscU (Vector xv_joined) a -> JoinSpine xv_joined xv_joined a
mkSpine uniq@(AscU v) = JoinSpine uniq (Wector ixs groups)
   where ixs = S.generate id
         groups = fmap (UnsafeFold1 . Unsized.singleton) ixs

-- | left join to an existing join spine. 
rejoin  :: forall a x__n m v__joined . (Ord a, KnownNat m, KnownNat v__joined) => JoinSpine x__n v__joined a -> Vector m a -> WectorFF Unsized.Vector x__n v__joined m
rejoin spine v' 
      | Wal grp' <- groupV v'
             -- get a unique represent for each group
             -- we assume that each groups are not empty
      =
             let AscU uniqV = jsSpine spine
                 uniqV' = walues grp' @=> v' 
                 -- foreach i in the left group we try to find the corresponding value 
                 -- in the right group and collect the index in grp'
                 -- ix'ac :: Vector v__joined (Maybe (Finite grp'))
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
                 -- expand :: Maybe (Finite grp') -> Unsized.Vector (Finite m)
                 expand gi'm = case gi'm of 
                                Nothing -> mempty
                                Just gi' -> unFold1 (walues grp' `S.index` gi')

             in Wector (windex $ jsGrouping spine) (expand <$> ix'ac) -- join


data JoinV xn xm = forall v__joined . KnownNat v__joined => JoinV (WectorFF Vector1 xn v__joined xn) (WectorFF Unsized.Vector xn v__joined xm) 
joinV :: (KnownNat xn, KnownNat xm, Ord a) => Vector xn a -> Vector xm a -> JoinV xn xm
joinV v v' | JoinSpineV spine <- makeJoinSpineV v
           = JoinV (jsGrouping spine) (rejoin spine v')
-- | Moving window
window :: KnownNat xv => Int -> WectorFF Vector1 xv xv xv
window size = let
    u = S.fromSized $ S.generate id
    windex = S.generate id
    walues = S.generate (\fi -> UnsafeFold1 $ Unsized.drop (fromIntegral fi + 1 - size) $ Unsized.take  (fromIntegral fi + 1) u)
    in Wector windex walues

main :: IO ()
main = withSizedList [ ("Adam-Navy", "Adam", 2)
                              , ("Adele-BLk", "Adele", 3)
                              , ("Adam-Black", "Adam", 10)
                              , ("Fiddle-Navy", "Fiddle", 64)
                              , ("Fiddle-Black", "Fiddle", 17)
                              , ("Fiddle-Blue", "Fiddle", 23)
                              ] $ \case
     sales | (sku, style, qty) <- S.unzip3 sales
           -> withSizedList [ ("Adam", "Plate")
                            , ("Fiddle", "Plate")
                            , ("Adele", "Cup")
                            ] $ \case
             style'shape 
              | (style_shape, shape_shape) <- S.unzip style'shape 

              , JoinV by_style on_style_ <- joinV style style_shape
              -> do
                  let Just on_style = traverse mkFold1 on_style_
                  print " ------- BY SHAPE ------ "
                  print on_style
                  mapM_ print $ S.zip sku $  wbroadcast on_style @=> style'shape -- shape_shape
                  print " ---- SUM by STYLE ---- "
                  let qty_style = sum <$> walues by_style @>$ qty
                  print qty_style
                  let shape = wbroadcast on_style @=> shape_shape
                  Wal by_shape <- return $ groupV shape 
                  print by_shape
                  let qty_shape = sum <$> walues by_shape @>$ qty
                  print qty_shape
                  mapM_ print $ S.zip4 sku
                                       (windex by_style @> qty_style)
                                       (wbroadcast on_style @=> shape_shape )
                                       (windex by_shape @> qty_shape)

                  print " ---- RUNNING ---- "
                  print $ rmap (unFold1 <$> walues by_shape) (S.postscanl' (+) 0)  Nothing qty 

                  print " --- Moving avegare -- "
                  let ma = window 2
                  print $ walues ma @>$ qty
                  print $ sum <$> walues ma @>$ qty
                  print " ==== Moving sorted === "
                  Wix by_sku <- return $ orderX sku 
                  let mo = crossCompose by_sku (window 2)
                  mapM_ print $ S.zip sales $  wbroadcast mo @>$ qty


            

     
_main = do
   withSizedList [("a", 2), ("c", 1), ("m", 6), ("w", 0), ("c", 3) , ("p", 0) ] $ \case 
      v'q | (v,q) <- S.unzip v'q
          , Wix s1_ <- selectX (fmap (`elem` ["c", "w"]) v)
          , s1 <- F.toList <$> s1_
          , Wix s2 <- selectWithX (Unsized.fromList . map snd
                                                . List.sort
                                                . map (\i -> (S.index v (S.index (windex s1) i), i))
                                                . Unsized.toList) (windex s1 @> v)
          -> do
                       print ("S1", s1)
                       let q1 = S.replicate $ S.sum $ windex s1 @> q -- :: Vector s1 Int
                       print ("S2", s2, windex s2 @> windex s1 @> v)
                       let q2 = S.iterateN (+1) 100 -- :: Vector s2 Int
                       print q2
                       
                       let s2' = composeWith (@>=) s2 $ F.toList <$> s1
                       print $ windex s2' @> q
                       print $ wexpand s2'  @>$ v'q
                       mapM_ print $ S.zip4 v q (walues s1 @>$ q1) (walues s2'  @>$ q2)
                       let s2m = traverse go s2'
                           go xs = case xs of 
                                      [x] -> Just (Just x)
                                      [] -> Just Nothing
                                      _ -> Nothing
                       case (s2m, traverse go s1) of
                          (Just s2M, Just s1M) -> mapM_ print $ S.zip3 v (walues s1M @>$ q1) (walues s2M  @>$ q2)
                          _ -> return ()
                           



        
