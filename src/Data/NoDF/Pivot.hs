{-# LANGUAGE BlockArguments, PatternSynonyms #-}
module Data.NoDF.Pivot where


import Data.NoDF.Wector
import Data.NoDF.Util
import Data.NoDF.Fold1
import Data.Vector qualified as V
import Data.Vector.Sized qualified as S
import Data.Vector.Sized (Vector, pattern SomeSized, fromSized) --  withSized, pattern SomeSized, Vector(..))
import Data.Finite
import qualified Data.Foldable as F
import qualified Data.Map as Map
import GHC.TypeNats --  (Nat, KnownNat)



{-| Unmelt a "table" given some key and var name. We don't need the variable value
   as we are only interested in the indices in the original table
   For example given

      | key     | var      |  value | ix
      Monday    | Patient  | 65     | 1
      Monday    | Recovery | 50     | 2
      Tuesday   | Patient  | 68     | 3
      Tuesday   | Recovery | 45     | 4
      Wednesday | Patient  | 70     | 5
      Thursday  | Recovery | 55     | 6

We want a map , for each var of columns (same length )
   The result should be a Maybe (Finite n), but only if we are sure that there are not duplicate.

     Patient =>  Monday    | 1 (65)
                 Tuesday   | 3
                 Wednesday | 5
                 Thursday  | Nothing
     Recovery => Monday    | 2 
                 Tuesday   | 4
                 Wednesday | Nothing
                 Thursday  | 6
                 
     and a list of keys Monday, Tuesday, Wednesday
     which could be  a grouping 
        Monday    : [1,2]
        Tuesday   : [3,4]
        Wednesday : [5]
        Thursday  : [6]
        
        
     or the spine


     
  
-}

data Pivot name x v__joined = Pivot { pvKeys :: WectorFF Vector1 x v__joined x
                                    , pvColumnMap :: Map.Map name (Vector v__joined (V.Vector (Finite x)))
                                    }
                               deriving (Show, Eq)
data PivotV name x = forall v . PivotV (Pivot name x v)
pivotV :: (Ord name, Ord key, KnownNat x) => Vector x key -> Vector x name -> PivotV name x
pivotV keyv varnamev | JoinSpineV spine <- makeJoinSpineV keyv
                     = PivotV $ Pivot (jsGrouping spine)
                                      (pivotWithSpine spine keyv varnamev)

pattern PivV keys colMap = PivotV (Pivot keys colMap)
-- pivoting :: (Ord name, Ord key, KnownNat n) => Vector n key -> Vector n name -> ( forall joined . KnownNat joined => Wector n joined (Vector1 (Finite n)) -> Map.Map name (Vector joined (V.Vector (Finite n))) -> r ) -> r
-- pivoting keyv varnamev f =  
--    -- first we collect all unique keys throught the whole vector
--    makingJoinSpine keyv \spine -> f (jsGrouping spine)
--                                     (pivotWithSpine spine keyv varnamev)
     {- the previous example 
        Monday    : [1,2]
        Tuesday   : [3,4]
        Wednesday : [5]
        Thursday  : [6]
     -}
-- | same as pivoting but take an already made spine.
-- This can be usefull if the spine is smaller than the spine which will 
-- be generated from pivoting. In that case only the used row are kept.
pivotWithSpine :: (Ord key, Ord name, KnownNat n, KnownNat joined) => JoinSpine m joined key -> Vector n key -> Vector n name -> Map.Map name (Vector joined (V.Vector (Finite n))) 
pivotWithSpine spine keyv varnamev 
   | indexv <-  S.generate id
   -- first we collect all unique keys throught the whole vector
     -- we then group by name then key so it
     -- could be segmented by var into ascending keys ready to be joined with the spine
     -- We are essentialy doing group by (ordering + segmenting) but sorting on two vector
     -- instead of one
   , Wix onVarKey <- orderX (Z2 varnamev keyv)
   , Wal byVarKey_ <- segmentV (windex onVarKey @> varnamev)
   =
         let key'n_byVar = walues byVarKey @>$ Z2 keyv indexv
             byVarKey = crossCompose onVarKey byVarKey_
         in -- f ( jsGrouping spine)
              Map.fromList [(varname, joined)
                             | (varname, Fold1 (SomeSized key'n)) <- F.toList $ Z2 (walues byVarKey @=> varnamev) key'n_byVar
                             , let Z2 key_ n_ = key'n
                             , let joinedN = rejoin spine key_
                             , let joined = (fmap (@> n_) (walues joinedN))
                             ]



         
