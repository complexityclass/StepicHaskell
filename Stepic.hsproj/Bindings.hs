module Bindings where
  
roots :: Double -> Double -> Double -> (Double, Double)
roots a b c =
  let d = sqrt (b ^ 2 - 4 * a * c)
      x1 = (-b - d) / (2 * a)
      x2 = (-b + d) / (2 * a) in
  (x1,x2)
  

factorial :: Int ->  Int
factorial n
  | n >= 0 = let
      helper acc 0 = acc
      helper acc n = helper (acc * n) (n - 1)
    in helper 1 n
  | otherwise = error "arg must be >= 0"
  

seqA :: Integer -> Integer
seqA n
  | n >= 0 = let
      helper acc1 acc2 acc3 0 = acc1
      helper acc1 acc2 acc3 1 = acc2
      helper acc1 acc2 acc3 2 = acc3
      helper acc1 acc2 acc3 n = helper (acc2) (acc3) (acc3 + acc2 - 2 * acc1) (n -1)
    in helper 1 2 3 n
  | otherwise = error "arg must be >= 0"
  
    
roots' :: Double -> Double -> Double -> (Double, Double)
roots' a b c = (x1, x2) where
  d = sqrt (b ^ 2 - 4 * a * c)
  x1 = (-b - d) / (2 * a)
  x2 = (-b + d) / (2 * a)
  

factorial' :: Integer -> Integer
factorial' n | n >= 0 = helper 1 n
             | otherwise = error "arg must be >= 0"
   where
     helper acc 0 = acc
     helper acc n = helper (acc * n) (n - 1)
     

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = let 
           helper sum count x 
             | (abs x) < 10 = (sum + abs(x), count)
             | otherwise = let remain = abs(mod x 10) in
                               helper (sum + remain) (count + 1) (div x 10)
           in helper 0 1 (abs x)
           

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * (f a + f b + innerSum) / 2
    where 
        h = (b - a) / 1000 
        innerPts  = map ((2*) . f . (a+)) (points (1000-1) h) 
        innerSum = sum innerPts

points  :: Double -> Double -> [Double]
points i x 
    | i <= 0 = []
    | otherwise = (i*x) : points (i-1) x
    


    






















            
          
