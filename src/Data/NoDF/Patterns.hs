{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
module Data.NoDF.Patterns where

import Data.NoDF.Wector  hiding(main)
import Data.Finite
import GHC.TypeNats --  (Nat, KnownNat)
import Data.Functor.Identity

-- For test, to remove
import qualified Data.Vector.Sized as S
import Data.Vector.Sized(Vector, index, pattern SomeSized, fromSized, withSizedList, imap)

data W_N_ n f = forall s .  KnownNat s => W_N_ (Wector s n (f (Finite s)))
data WN_ n f = forall grp . WN_ (Wector n grp (f (Finite n)))


selectW v = selecting v W_N_
pattern SelectW wector <- (selectW -> W_N_ wector) 

filterW f v = filtering f v W_N_
pattern FilterW wector <- (uncurry filterW -> W_N_ wector)

takeW n v = taking n v W_N_
pattern TakeW wector <- (uncurry takeW -> W_N_ wector)

dropW n v = dropping n v W_N_
pattern DropW wector <- (uncurry dropW -> W_N_ wector)
     
takeWhileW n v = takingWhile n v W_N_
pattern TakeWhileW wector <- (uncurry takeWhileW -> W_N_ wector)

dropWhileW n v = dropping n v W_N_
pattern DropWhileW wector <- (uncurry dropW -> W_N_ wector)


orderW v = ordering v W_N_
pattern OrderW wector <- (orderW -> W_N_ wector) 

segmentW v = segmenting v WN_
pattern SegmentW wector <- (segmentW  -> WN_ wector)

groupW v = grouping v WN_
pattern GroupW wector <- (groupW -> WN_ wector)

pattern WSomeIx wector <- ( ($ W_N_) -> W_N_ wector)
pattern WSomeItems wector <- ( ($WN_ ) -> W_N_ wector)

--- * Test
main :: IO ()
main = do
  withSizedList [ ("Adam-Navy", "Adam", 2)
                , ("Adele-BLk", "Adele", 3)
                , ("Adam-Black", "Adam", 10)
                , ("Fiddle-Navy", "Fiddle", 64)
                , ("Fiddle-Black", "Fiddle", 17)
                , ("Fiddle-Blue", "Fiddle", 23)
                ] $ \sales -> do
    let (n_sku, n_style, n_qty) = S.unzip3 sales
    case () of 
        _  | WSomeIx qNnQ <- filtering odd n_qty
           , WSomeItems qSsQQ <- segmenting $ windex qNnQ @> n_style
           , let qN = walues qSsQQ @>$ windex qNnQ
           -> do 
               mapM print $ walues qSsQQ @>$ (windex qNnQ @> n_qty )
               mapM print $ S.zip (qN @=> n_style) (qN @>$ n_qty)
               mapM_ print $ windex qNnQ @> sales
