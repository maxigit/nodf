{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Data.NoDF
( module Data.NoDF.Wector
, module Data.NoDF
, module Data.Vector.Sized
, module GHC.TypeNats
)
where 

import Data.NoDF.Wector hiding(main)
import Data.Vector.Sized (fromSized, withSized, withSizedList, pattern SomeSized, Vector(..), index, imap )
import qualified Data.Vector.Sized  as S
import GHC.TypeNats

{-# COMPLETE Z2, Z3, Z4, Z5, Z6 #-}
pattern Z2 :: Vector n a -> Vector n b -> Vector n (a,b)
pattern Z2 a b <- (S.unzip -> (a, b)) where
   Z2 a b = S.zip a b
pattern Z3 a b c <- (S.unzip3 -> (a, b, c)) where
   Z3 a b c = S.zip3 a b c
pattern Z4 a b c d <- (S.unzip4 -> (a, b, c, d)) where
   Z4 a b c d = S.zip4 a b c d
pattern Z5 a b c d e <- (S.unzip5 -> (a, b, c, d, e)) where
   Z5 a b c d e = S.zip5 a b c d e
pattern Z6 a b c d e f <- (S.unzip6 -> (a, b, c, d, e, f)) where
   Z6 a b c d e f = S.zip6 a b c d e f

