module Data.NoDF.Operators (
module Export
)
where

import Data.NoDF.Wector as Export ((@>), (@>$), (@>=), (@>~), (@=>), (>.>), (>.<))
import Data.NoDF.Wector

(Wector ix _) @_> v  = ix @> v
(Wector _ items) _@> v  = ix @> v

(W x items) @@>$ v = x @> items @>$ v
