{-# LANGUAGE GADTs #-}
module PVector where


import qualified Data.Vector.Generic as G
data JVector v df a where
   JColumn  :: G.Vector v a => v a -> JVector v df a
   JConstant :: a -> JVector v df a

main :: IO ()
main = do
 print "hello"
