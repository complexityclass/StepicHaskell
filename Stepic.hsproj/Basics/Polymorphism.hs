module Basics.Polymorphism where
import Data.Function
  
--Parametric Polymorphism

getSecondFrom :: a -> b -> c -> b
getSecondFrom _ x _ = x

mono :: Char -> Char
mono x = x

semiMono :: Char -> a -> Char
semiMono x y = x

apply2 f x = f (f x)

---
multSecond = g `on` h
g = (*)
h = snd

---
lenVec x y = sqrt $ x ^ 2 + y ^ 2
lenVec' x = \y -> sqrt $ x ^ 2 + y ^ 2
lenVec'' = \x -> \y -> sqrt $ x ^ 2 + y ^ 2
lenVec''' = \x y -> sqrt $ x ^ 2 + y ^ 2

sumFstFst = (+) `on` (\pp -> fst $ fst pp)

---
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

sum3squares = (\x y z -> x + y + z) `on3` (^2)

---
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g = \x -> f (g x)

---

sumFstFst' = (+) `on` (fst . fst)

---
doItYourself = f' . g' . h'

f' = (logBase 2)

g' = (^ 3)

h' = (max 42.0)


---

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b list = a : b : list

nTimes:: a -> Int -> [a]
nTimes x times = let 
   f x acc count 
     | count == 0 = acc
     | otherwise = f x (x : acc) (count - 1)
   in f x [] times 
   

---

avg :: (Double, Double) -> Double
avg p = (fst p + snd p) / 2

---

swap2 = uncurry (flip (,))








