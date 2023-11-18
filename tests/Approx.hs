module Approx 
  ( assertApproxZero ) 
  where
import Test.Tasty.HUnit ( Assertion, assertEqual )

-- round x to n digits
approx :: Int -> Double -> Double
approx n x = fromInteger (round $ x * (10^n)) / (10.0^^n)

assertApproxZero :: String -> Int -> Double -> Assertion
assertApproxZero prefix n x = assertEqual prefix (approx n x) 0.0
