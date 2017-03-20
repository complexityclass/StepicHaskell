module Lists.ListFunctions where

import Data.Char  

readDigits :: String -> (String, String)
readDigits = span isDigit  

---

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x:xs)
    | (p1 x) || (p2 x) = x : filterDisj p1 p2 xs
    | otherwise = filterDisj p1 p2 xs

---    

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort list = let 
    index  = (length list) `div` 2
    pivot  = list !! index
    less   = filter (< pivot) list
    equal  = filter (== pivot) list
    greater = filter (> pivot) list 
    in (qsort less) ++ equal ++ (qsort greater)

---

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

---
perms :: [a] -> [[a]]
perms [x] = [[x]]
perms list = concatMap []

    
