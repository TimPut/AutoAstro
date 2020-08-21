{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StrictData           #-}

module Main (main) where

import AutoAstro
import           Codec.Picture
import           Data.List                    (foldl', nub, sort, sortBy, sortOn)
import           System.Environment           (getArgs)
import           System.TimeIt



main = timeIt $ do

  [in1, in2, out] <- getArgs :: IO [FilePath]

  -- Read in images
  Image !w !h !d <- getImg in1
  Image !w2 !h2 !d2 <- getImg in2

  -- Pack channels into carrays for use by FFTW
  let p1@(p10,p11,p12) = splitColors d
      (r1,g1,b1) = ((toComplexArray' id (w) (h)) . fmap fromIntegral) `mapTriple` p1
      -- lum = toComplexArray' id w h $ zipWith3 ave3 p10 p11 p12
  let p2 = splitColors d2
      (r2,g2,b2) = ((toComplexArray' id (w) (h)) . fmap fromIntegral) `mapTriple` p2

  -- Compute phasecorrelation
  let offsets p q = fmap (\((a,b),c) -> ((modDist (w) a, modDist (h) b),c)) $ phaseCorrelation (w) (h) p q
      r_off = offsets r1 r2
      g_off = offsets g1 g2
      b_off = offsets b1 b2

  -- (Heuristically) Choose likely translation parameters as weighted-RMS of top 10 best fit translations.
  let fit os = (\(a,b,r) -> if r == 0 then (0,0) else (a/r,b/r))
               . foldl' (\(a,b,c) (x,y,z) -> (a+x,b+y,c+z)) (0,0,0)
               . fmap (\((a,b),r) -> (fromIntegral a * r**2, fromIntegral b * r**2, r**2))
               . take 10 $ os

      ro = fit r_off
      go = fit g_off
      bo = fit b_off

  -- Per-channel offsets in fractional pixels
  print ro
  print go
  print bo

  let r' = shiftFourier w h (fst ro) (snd ro) r2
      g' = shiftFourier w h (fst go) (snd go) g2
      b' = shiftFourier w h (fst bo) (snd bo) b2

  let result = toImageRGB16 w h (r',g',b')
  writeTiff out result

