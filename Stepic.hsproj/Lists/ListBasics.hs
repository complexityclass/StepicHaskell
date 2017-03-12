module Lists.ListBasics where
  
import Prelude hiding (length, (++), null)
  
cons42 = (42 :)

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b list = a : b : list

nTimes :: a -> Int -> [a]
nTimes elem n = let 
  helper acc value count 
      | count == 0 = acc
      | otherwise = helper (value : acc) value (count - 1)
    in helper [] elem n
    

second :: [a] -> a
second xs = head (tail xs)

fst' ((,) x y) = x

head' ((:) x xs) = x

tail' (_:xs) = xs

second' :: [a] -> a
second' (_:xs) = head xs

second'' :: [a] -> a
second'' (_ : x : _) = x

sndHead = snd . head

t ((:) ((,) _ x) y) = x


length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs)
  | odd x  = x : (oddsOnly xs)
  | otherwise = oddsOnly xs 
  

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome list = list == (reverse list)

sum2 :: Num a => [a] -> [a] -> [a]
sum2 [] []  = []
sum2 [] list = list
sum2 list [] = list
sum2 [x] [y] = [(x+y)]
sum2 [x] (y:ys) = (x+y) : ys
sum2 (x:xs) [y] = (x+y) : xs
sum2 (x:xs) (y:ys) = (x + y) : (sum2 xs ys) 

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 x y z = sum2 (sum2 x y) z

-- Task 3.13

groupElems :: Eq a => [a] -> [[a]]
groupElems list = combineEquals $ (map (\x -> [x]) list) 

combineEquals :: Eq a => [[a]] -> [[a]]
combineEquals [] = []
combineEquals [[x]] = [[x]]
combineEquals [[x],[y]]
    | x == y = [[x,y]]
    | otherwise = [[x],[y]]
combineEquals (h:s:xs)
    | (head h) == (head s) = combineEquals $ ((head s) : h) : xs
    | otherwise = h : (combineEquals (s:xs))
    
--






     

