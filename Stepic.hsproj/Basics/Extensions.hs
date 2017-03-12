module Basics.Extensions where
  
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool
    
class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab kgd 
      | (doesEnrageGork kgd) && (doesEnrageMork kgd) = stomp (stab kgd) 
      | doesEnrageGork kgd = stab kgd
      | doesEnrageMork kgd = stomp kgd
      | otherwise = kgd
      

a = 127.2 :: Double
b = 24.1 :: Double
c = 20.1 :: Double
d = 2
 
ip = show a ++ show b ++ show c ++ show d

---

class (Bounded a, Enum a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc r 
    | r == maxBound = minBound
    | otherwise = succ r

  spred :: a -> a
  spred = undefined