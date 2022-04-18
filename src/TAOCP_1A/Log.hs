module TAOCP_1A.Log where

log :: Double -> Double -> Double
log b x = a + (num 0.5 $ take 31 $ bits b x0)
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
