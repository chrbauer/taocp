module TAOCP_1A.Log where

import Prelude hiding (log)
import Data.Array

log :: Double -> Double -> Double
log b x = a + (num 0.5 $ take 52 $ bits b x0)
  where (a, x0) =  split b x 0 

split :: Double -> Double -> Double -> (Double, Double)
split b x u 
  | x < b     = (u, x)
  | otherwise = split b (x/b) (1+u)
          

bits :: Double -> Double -> [Bool]
bits b x
  | x2 > b = True : bits b (x2/b)
  | otherwise = False : bits b x2
 where x2 = x * x
 

num _ [] = 0
num d (False:t) = num (d/2) t
num d (True:t) = d+num (d/2) t


l :: Double -> Double -> Double
l b x 
  | x < 1 || x >= 2 = error "x"
  | otherwise       = go x 0 (shiftR x 1) 1
  where go x y z k
          | x == 1.0 || k > 53 = y
          | x - z < 1 = go x y (shiftR z 1) (k+1)
          | otherwise = let x' = x-z in go x' (y + tab ! k) (shiftR x' k) k
        shiftR x k = x / (2.0 ^ k)
        tab     = array (1, 128) [(k, log b (2^k/(2^k-1))) | k <- [1..53]]

lg = l 2
