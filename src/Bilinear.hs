module Bilinear where

import Linear

bilinear :: Num a => V2 a -> a -> a -> a -> a -> a
bilinear (V2 x y) a b c d = (V2 (1 - y) y) `dot` (V2 (V2 a b) (V2 c d) !* (V2 (1 - x) x))

bilinear :: Num a => V2 a -> a -> a -> a -> a -> a
bilinear (V2 x y) a b c d = (V2 (1 - y) y) `dot` (V2 (V2 a b) (V2 c d) !* (V2 (1 - x) x))
