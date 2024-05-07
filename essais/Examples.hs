module Examples
  where
import Data.Ratio           ( (%) )
import Data.VectorSpace     ( AdditiveGroup((^-^)), VectorSpace((*^)) )
import Geometry.VertexEnum

testSmall :: [Constraint Double]
testSmall = [ x .<= 1, x .>= 0, y .<= 1]
  where
    x = newVar 1
    y = newVar 2

rggConstraints :: [Constraint Double]
rggConstraints =
  [ x .>= (-5)
  , x .<=  4
  , y .>= (-5)
  , y .<=. cst 3 ^-^ x
  , z .>= (-10)
  , z .<=. cst 6 ^-^ x ^-^ y ]
  where
    x = newVar 1
    y = newVar 2
    z = newVar 3

-- region3D :: [Constraint Double]
-- region3D =
--   [ x .>=  0 -- shortcut for x .>=. cst 0
--   , x .<=  3
--   , y .>=  0
--   , y .<=. cst 2 ^-^ (2%3)*^x
--   , z .>=  0
--   , z .<=. cst 6 ^-^ 2*^x ^-^ 3*^y ]
--   where
--     x = newVar 1
--     y = newVar 2
--     z = newVar 3

cubeConstraints :: [Constraint Double]
cubeConstraints =
  [ x .<= 1
  , x .>= (-1)
  , y .<= 1
  , y .>= (-1)
  , z .<= 1
  , z .>= (-1) ]
  where
    x = newVar 1
    y = newVar 2
    z = newVar 3

cubeConstraints' :: [[Double]]
cubeConstraints' = [[ 1, 0, 0,-1]
                   ,[-1, 0, 0,-1]
                   ,[ 0, 1, 0,-1]
                   ,[ 0,-1, 0,-1]
                   ,[ 0, 0, 1,-1]
                   ,[ 0, 0,-1,-1]]
