{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StrictData           #-}



{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AutoAstro
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


import           Data.Array.CArray
import           Data.Complex

import           Data.List                    (foldl', nub, sort, sortBy, sortOn)
import           Data.List.Split
import           Data.Vector.Storable         ((!))
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as M


--Scaling
import qualified Codec.Picture.Types          as M
import           Control.Monad.ST

import qualified Data.Massiv.Array            as M
-- import           Fastro
-- import qualified Fastro.Entries               as F
-- import           Fastro.Types

ave3 :: (Integral a) => a -> a -> a -> Double
ave3 r g b = ((fromIntegral r / 3) + (fromIntegral g / 3) + (fromIntegral b / 3))

-- Translate a w*h image by the vector (x,y) in frequency space.
shiftFourier :: Int -> Int -> Double -> Double -> CArray (Int,Int) (Complex Double) -> CArray (Int,Int) (Complex Double)
shiftFourier w h x y img = array ((0,0),(w-1,h-1)) . fmap shift . assocs $ img
    where
      shift ((u,v),p) = ((u,v),p*exp((-i)*2*pi*(fromIntegral ((indexShift w u)) * x_0 + fromIntegral ((indexShift h v)) * y_0) + 0*pi*i*(toComplex x + toComplex y)))
      h' = fromIntegral h
      w' = fromIntegral w
      x_0 = toComplex $ x/w'
      y_0 = toComplex $ y/h'
      i = 0 :+ 1
      indexShift m n = if n >= (m `div` 2) then (n - m) + 1 else n

modDist m n = if n > (m `div` 2) then (n - m) else n

-- main1 :: IO ()
-- main1 = do
  -- putStrLn "Running"
  -- ctx <- getContext []
  -- print $ runFTIn ctx $ fromFVectorPair $ mPr F.idft $ mOp F.dft a b
  -- return ()
  -- where a = toFVector $ M.fromLists' M.Par [[1,0,-1,0 :: Double],[0,0,0,0 :: Double]]
        -- b = toFVector $ M.fromLists' M.Par [[0,0,0,0 :: Double],[0,0,0,0 :: Double]]

shiftFFTW w h x y = idftN [0,1] . shiftFourier w h (x) (y) . dftN [0,1]

toImageRGB16 w h rgb = Image w h
                       ( joinColors
                       . mapTriple (fmap ( round
                                         . clamp (2**16-1)
                                         . realPart)
                                   . elems)
                       $ rgb) :: Image PixelRGB16

getImg :: FilePath -> IO (Image PixelRGB16)
getImg fp = do
  e <- readImage fp
  case e of
    Right (ImageRGB16 img) -> return (img)
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
-- returns the number of pixels imga must be shifted to overlap imgb
phaseCorrelation w h imga imgb = fmax' . assocs . amap (realPart . abs) $ phaseCorrelation' w h imga imgb
    where
      fmax []         = (0,0)
      fmax ((i,e):xs) = go i e xs
      go i e []         = i
      go i e ((j,x):xs) = if x > e then go j x xs else go i e xs
      fmax' xs = sortBy (\a b -> compare (snd b) (snd a)) xs

-- Note that this implementation of Tukey windowing is zero indexed
tukey a k n = if abs(k'-m) >= a*m
              then (1+cos((pi*(abs (k'-m) - a*m))/((1-a)*m)))/2
              else 1
    where
      m = (fromIntegral n -1)/2
      k' = fromIntegral k

hamming n m = (sin (pi * (fromIntegral n) / (fromIntegral m)))**2

phaseCorrelation' w h imga imgb = idftN [0,1] (listArray ((0,0),(w-1,h-1)) crossSpectrum)
    where
      -- windowFunc = \a b -> toComplex $ hamming a b
      windowFunc = \a b -> toComplex $ (tukey 0.7) a b
      window []             = []
      window (((i,j),x):xs) = ((i,j),(windowFunc i w * windowFunc j h * x)):window xs
      crossSpectrum = zipWith (\i j -> let p = (i * j) in p / abs p) g_a g_b
      g_a = f imga
      g_b = fmap conjugate $ f imgb
      f img = elems . dftN [0,1] . array ((0,0),(w-1,h-1)) . window . assocs $ img


-- findMax :: (Ord a) => Int -> Int -> [a] -> (Int, Int)
-- findMax w h [] = (0,0)
-- findMax w h (x:xs) = let i = (go x 0 0 xs) in (i `mod` w, i `quot` h)
    -- where
      -- go _ i _ []       = i
      -- go x !i !j (y:xs) = if y > x then go y j (j+1) xs else go x i (j+1) xs


toComplexArray' :: (Double -> Double) -> Int -> Int -> [Double] -> CArray (Int,Int) (Complex Double)
toComplexArray' f w h d = listArray ((0,0),(w-1,h-1)) (toComplex . f <$> d)

toComplex :: (Num a) => a -> Complex a
toComplex r = r :+ 0

splitColors :: (M.Storable a) => V.Vector a -> ([a],[a],[a])
splitColors as = (r,g,b)
  where
    getC []     = []
    getC (c:as) = c:getC (drop 2 as)
    r = getC (V.toList as)
    g = getC . drop 1 $ (V.toList as)
    b = getC . drop 2 $ (V.toList as)

joinColors :: (M.Storable a) => ([a],[a],[a]) -> V.Vector a
joinColors (r,g,b) = V.fromList (interleave3 r g b)
  where
    interleave3 [] _ _               = []
    interleave3 _ [] _               = []
    interleave3 _ _ []               = []
    interleave3 (r:rs) (g:gs) (b:bs) = r:g:b:interleave3 rs gs bs

clamp n x = min n x
mapTriple f (a,b,c) = (f a, f b, f c)

evens []        = []
evens [x]       = [x]
evens (a0:_:as) = a0:evens as
odds []        = []
odds [x]       = []
odds (_:b1:bs) = b1:odds bs

--dht' v xs = dht' v (evens xs) + dht' (odds xs) + dht' (reverse . odds xs)

--------------------------------------------------------------------------------
-- Utilities for Generated Futhark bindings in FAstro
--------------------------------------------------------------------------------
{-
mOp :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
mOp mf ma mb = do
    a <- ma
    b <- mb
    mf a b

mPr :: (Monad m) => (a -> b -> m c) -> m (a, b) -> m c
mPr mf mab = do
    (a,b) <- mab
    ab <- mf a b
    pure ab

type FVector c = FT c (F64_2d c)
toFVector :: M.Array M.S M.Ix2 Double -> FVector c
toFVector = toFuthark

fromFVector :: FVector c -> FT c (M.Array M.S M.Ix2 Double)
fromFVector = (>>= fromFuthark)

type FVectorPair c = FT c (F64_2d c, F64_2d c)
toFVectorPair :: (M.Array M.S M.Ix2 Double,M.Array M.S M.Ix2 Double) -> FVectorPair c
toFVectorPair = toFutharkT2

fromFVectorPair :: FVectorPair c -> FT c (M.Array M.S M.Ix2 Double,M.Array M.S M.Ix2 Double)
fromFVectorPair = (>>= fromFutharkT2)
-}
--------------------------------------------------------------------------------
