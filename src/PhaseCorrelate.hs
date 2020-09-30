{-# LANGUAGE FlexibleContexts #-}

module PhaseCorrelate where

import           Math.FFT
import           Math.FFT.Base (FFTWReal)
import           Data.Array.CArray
import           Data.Complex

phaseCorrelation
  :: (Integral a1, Ix a1, Math.FFT.Base.FFTWReal r,
      Shapable (a1, a1), IArray a2 (Complex r), IArray a3 (Complex r)) =>
     a1
     -> a1
     -> a2 (a1, a1) (Complex r)
     -> a3 (a1, a1) (Complex r)
     -> CArray (a1, a1) (Complex r)
phaseCorrelation w h imga imgb = idftN [0,1] (listArray ((0,0),(w-1,h-1)) crossSpectrum)
    where
      -- windowFunc = \a b -> toComplex $ hamming a b
      windowFunc = \a b -> (\r -> r :+ 0) $ (tukey 0.7) a b
      window []             = []
      window (((i,j),x):xs) = ((i,j),(windowFunc i w * windowFunc j h * x)):window xs
      crossSpectrum = zipWith (\i j -> let p = (i * j) in p / abs p) g_a g_b
      g_a = f imga
      g_b = fmap conjugate $ f imgb
      f img = elems . dftN [0,1] . array ((0,0),(w-1,h-1)) . window . assocs $ img

-- Note that this implementation of Tukey windowing is zero indexed
tukey :: (Floating p, Integral a1, Integral a2, Ord p)
      => p -> a1 -> a2 -> p
tukey a k n = if km >= am
              then (1+cos((pi*(km - am))/((1-a)*m)))/2
              else 1
    where
      m = (fromIntegral n -1)/2
      k' = fromIntegral k
      am = a*m
      km = abs(k' - m)
{-# INLINE tukey #-}

hamming :: (Floating a1, Integral a2, Integral a3) => a2 -> a3 -> a1
hamming n m = (sin (pi * (fromIntegral n) / (fromIntegral m)))**2
