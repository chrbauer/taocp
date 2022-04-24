module TAOCP_1A.Log
  (log, log10, ln, lg)
where

import Prelude hiding (log)
import Data.Array

logG :: (Floating a, Ord a) => a -> a -> a
logG b x = a + (num 0.5 $ take 52 $ bits b x0)
  where (a, x0) =  split b x 0 

split :: (Floating a, Ord a) => a -> a -> a -> (a, a)
split b x u 
  | x < b     = (u, x)
  | otherwise = split b (x/b) (1+u)
          

bits :: (Floating a, Ord a) => a -> a -> [Bool]
bits b x
  | x2 > b = True : bits b (x2/b)
  | otherwise = False : bits b x2
 where x2 = x * x
 

num _ [] = 0
num d (False:t) = num (d/2) t
num d (True:t) = d+num (d/2) t

-- we want to keep tab for log10, lg and ln 
{-# INLINABLE log #-}
log :: (Floating a, Ord a) => a -> a -> a
log b x 
  | x <= 0 = error "x <= 0"
  | x < 1 = -(log b (1/x))
    --let eps = 1 - x in 
    --  feynman (1-x) (b ** (1-eps)) 1
  | x >= 2 = 1 + log b (x/b)
  | otherwise       = briggs x 0 (shiftR x 1) (1 :: Int)
  where briggs x y z k
          | x == 1.0 || k > 53 = y
          | x - z < 1 = briggs x y (shiftR z 1) (k+1)
          | otherwise = let x' = x-z in briggs x' (y + tab ! k) (shiftR x' k) k
        shiftR x k = x / (2.0 ^ k)
        tab     = array (1, 128) [(k, logG b (2^k/(2^k-1))) | k <- [1..53]]
        -- TODO: find the error
        feynman x y k 
          | x == 1 || k > 50 = y
          | x < tab ! k      = feynman x y (k+1)
          | otherwise        = feynman (x - tab ! k) (y - shiftR y k) k
        

e = exp 1
log10 = log 10
lg = log 2
ln = log e
