module Data.NoDF.Operators (
module Export
, module Data.NoDF.Operators
)
where

import Data.NoDF.Wector as Export ((@>), (@>$), (@>=), (@>~), (@=>), (>.>), (>.<))
import Data.NoDF.Wector

(Wector ix _) @~> v  = ix @> v
(Wector _ items) ~@> v  = items @> v
(Wector _ items) ~@>$ v  = items @>$ v

(Wector x items) @@>$ v = x @> items @>$ v
