module Basics.TypeClass where

class MyEq a where
  (|==|), (|/=|) :: a -> a -> Bool
  x |/=| y = not (x |==| y)


instance MyEq Bool where
  True  |==| True  = True
  False |==| False = False
  _     |==| _     = False
  

---

class Printable p where
  toString :: p -> String
  
instance Printable Bool where
  toString True = "true"
  toString False = "false"
  
instance Printable () where
  toString () = "unit type"
  
---

instance (Eq a, Eq b) => MyEq (a,b) where
  p1 |==| p2 = fst p1 == fst p2 && snd p1 == snd p2
  
---

instance (Printable a, Printable b) => Printable (a,b) where
  toString (a,b) = "(" ++ (toString a) ++ "," ++ (toString b) ++ ")"

