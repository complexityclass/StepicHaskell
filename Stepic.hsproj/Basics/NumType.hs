module Basics.NumType where
  
avg :: Int -> Int -> Int -> Double
avg a b c = let ad = fromIntegral a :: Double
                bd = fromIntegral b :: Double
                cd = fromIntegral c :: Double 
             in (ad + bd + cd) / 3.0
             
                
