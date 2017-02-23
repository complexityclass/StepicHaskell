module Intro.Recursion where
  
factorial n = if n == 0 then 1 else n * factorial (n - 1)

factorial' 0 = 1
factorial' n = if n < 0 then error "arg must be > 0" else  n * factorial' (n - 1)

doubleFact :: Integer -> Integer
doubleFact 1 = 1
doubleFact 2 = 2
doubleFact n = n * doubleFact (n - 2) 

factorial''' 0 = 1
factorial''' n | n < 0 = error "arg must be >= 0"
               | n > 0 = n * factorial''' (n - 1)
               
factorial4 :: Integer -> Integer
factorial4 n | n == 0    = 1
             | n > 0     = n * factorial4 (n - 1)
             | otherwise = error "arg must be >= 0"
             

fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' (-1) = 1
fibonacci' n | n > 1  = fibonacci' (n - 1) + fibonacci' (n - 2)
            | n < -1 = - (fibonacci' (n + 1) - fibonacci' (n + 2))
            

factorial5 n | n >= 0    = helper 1 n
             | otherwise = error "arg must be > = 0" 

helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)


fibonacci :: Integer -> Integer
fibonacci n = fibHelper 1 1 n

fibHelper :: Integer -> Integer -> Integer -> Integer
fibHelper acc1 acc2 n | n == 0  = 0
                      | n == 1  = acc1
                      | n == -1 = acc1
                      | n > 1   = fibHelper (acc2) (acc1 + acc2) (n - 1)
                      | n < -1  = fibHelper (-acc2) ((-acc1) + (-acc2)) (n + 1) 
            
               


