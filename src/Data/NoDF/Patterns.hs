{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module Data.NoDF.Patterns where

import Data.NoDF.Wector 
import Data.Finite
import Data.Functor.Identity

data W_N_ n f = forall s .  W_N_ (Wector s n (f (Finite s)))
data WN_ n f = forall grp .  WN_ (Wector n grp (f (Finite n)))


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
-- * 
