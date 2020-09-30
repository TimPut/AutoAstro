{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StrictData           #-}



{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AutoAstro (shiftFourier, splitColors, getImg, toComplexArray, mapTriple, modDist, bestFit, toImageRGB16)
        where

--------------------------------------------------------------------------------
-- JuicyPixels imports
--------------------------------------------------------------------------------
import           Codec.Picture
--import           Codec.Picture.Types

--------------------------------------------------------------------------------
-- FFTW imports
--------------------------------------------------------------------------------
import           Math.FFT
import           Math.FFT.Base (FFTWReal)

import           Data.Array.CArray
import           Data.Complex

import           Data.List                    (sortBy)
import           Data.List.Split
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as M

import PhaseCorrelate

-- Translate a w*h image by the vector (x,y) in frequency space.
shiftInPhaseSpace :: Double -> Double -> CArray (Int,Int) (Complex Double) -> CArray (Int,Int) (Complex Double)
shiftInPhaseSpace x y img = array ((0,0),(w-1,h-1)) . fmap shifted . assocs $ img
    where
      [w,h] = shape img
      shifted ((u,v),p) = ((u,v)
                          ,p*exp((-i)*2*pi
                                 * (fromIntegral (indexShift w u) * x_0
                                    + fromIntegral (indexShift h v) * y_0)))
      h' = fromIntegral h
      w' = fromIntegral w
      x_0 = (x/w') :+ 0
      y_0 = (y/h') :+ 0
      i = 0 :+ 1
      indexShift m n = if n <= (m `div` 2) then n else (n-m)

main2 = id --(\xs -> (take 4 xs, drop 4 xs))
        . fmap ((/100) . fromIntegral . round . (*100))
        . fmap realPart
        . elems
        . shiftFourier (-1.4) (-0.4)
        $ (listArray ((0,0),(2,2)) [1,0,0,0,0,0,0,0,0,0,0,0])

modDist :: Integral p => p -> p -> p
modDist m n = if n > (m `div` 2) then (n - m) else n

shiftFourier :: Double
             -> Double
             -> CArray (Int, Int) (Complex Double)
             -> CArray (Int, Int) (Complex Double)
shiftFourier x y = idftN [0,1] . shiftInPhaseSpace (x) (y) . dftN [0,1]

toImageRGB16 :: (RealFrac a1, Floating a1, IArray a2 (Complex a1), Ix i)
             => Int
             -> Int
             -> (a2 i (Complex a1), a2 i (Complex a1), a2 i (Complex a1))
             -> Image PixelRGB16
toImageRGB16 w h rgb = Image w h
                       ( joinColors
                       . mapTriple (fmap ( round
                                         . clamp (2**16-1)
                                         . realPart)
                                   . elems)
                       $ rgb) :: Image PixelRGB16
    where
      clamp n x = min n x

mapTriple :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTriple f (a,b,c) = (f a, f b, f c)
{-# INLINE mapTriple #-}

getImg :: FilePath -> IO (Image PixelRGB16)
getImg fp = do
  e <- readImage fp
  case e of
    Right (ImageRGB16 img) -> return (img)
    Right _                -> error "Unexpected image format"
    Left err               -> error err

-- Width -> Height -> H-Shift -> V-Shift -> (Img -> Img)
shift :: (Num a) => Int -> Int -> Int -> Int -> [a] -> [a]
shift w h lr ud img = concat .
                      fmap (if ud >= 0
                       then \xs -> drop ud xs ++ (replicate ud 0)
                       else \xs -> replicate (abs ud) 0 ++ take (w - abs ud) xs) .
                      (if lr >= 0
                       then \xs -> drop lr xs ++ replicate lr (replicate w 0)
                       else \xs -> replicate (abs lr) (replicate w 0) ++ take (h - abs lr) xs) $
                      chunksOf w img


-- https://en.wikipedia.org/wiki/Phase_correlation
-- returns descending (by fit) list of candidate numbers of pixels imga must be shifted to overlap imgb
bestFit :: (Ix a1, Integral a1, Math.FFT.Base.FFTWReal b,
           Shapable (a1, a1), IArray a2 (Complex b), IArray a3 (Complex b))
        => a1
        -> a1
        -> a2 (a1, a1) (Complex b)
        -> a3 (a1, a1) (Complex b)
        -> [((a1, a1), b)]
bestFit w h imga imgb = fmax . assocs . amap (realPart . abs) $ phaseCorrelation w h imga imgb
    where
      fmax xs = sortBy (\a b -> compare (snd b) (snd a)) xs

toComplexArray :: (Double -> Double) -> Int -> Int -> [Double] -> CArray (Int,Int) (Complex Double)
toComplexArray f w h d = listArray ((0,0),(w-1,h-1)) ((\r -> r :+ 0) . f <$> d)

splitColors :: (M.Storable a) => V.Vector a -> ([a],[a],[a])
splitColors as = (r,g,b)
  where
    getC []     = []
    getC (c:cs) = c:getC (drop 2 cs)
    r = getC (V.toList as)
    g = getC . drop 1 $ (V.toList as)
    b = getC . drop 2 $ (V.toList as)

joinColors :: (M.Storable a) => ([a],[a],[a]) -> V.Vector a
joinColors (rl,gl,bl) = V.fromList (interleave3 rl gl bl)
  where
    interleave3 [] _ _               = []
    interleave3 _ [] _               = []
    interleave3 _ _ []               = []
    interleave3 (r:rs) (g:gs) (b:bs) = r:g:b:interleave3 rs gs bs


