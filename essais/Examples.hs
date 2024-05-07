module Examples
  where
import Data.Ratio           ( (%) )
import Data.VectorSpace     ( AdditiveGroup((^-^), (^+^)), VectorSpace((*^)) )
import Geometry.VertexEnum

testSmall :: [Constraint Rational]
testSmall = [ x .<= (1), y .<= (1), x .>= 0, y .>= 0]
  where
    x = newVar 1 
    y = newVar 2 

testSmall' :: [Constraint Rational]
testSmall' = [ x .<= (-1), x .>= (-2), y .<= (-1)]
  where
    x = newVar 1
    y = newVar 2

rggConstraints :: [Constraint Rational]
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

region3D :: [Constraint Rational]
region3D =
  [ x .>=  0 -- shortcut for x .>=. cst 0
  , x .<=  3
  , y .>=  0
  , y .<=. cst 2 ^-^ (2%3)*^x
  , z .>=  0
  , z .<=. cst 6 ^-^ 2*^x ^-^ 3*^y ]
  where
    x = newVar 1
    y = newVar 2
    z = newVar 3

cube :: [Constraint Rational]
cube =
  [ x .<= (-1)
  , x .>= (-2)
  , y .<= 1
  , y .>= (-1)
  , z .<= 1
  , z .>= (-1) ]
  where
    x = newVar 1 -- ^-^ cst (1)
    y = newVar 2 -- ^-^ cst (1)
    z = newVar 3 -- ^-^ cst (1)

cubeConstraints :: [Constraint Rational]
cubeConstraints =
  [ x .<= 1
  , x .>= (-1)
  , y .<= 1
  , y .>= (-1)
  , z .<= 1
  , z .>= (-1) ]
  where
    x = newVar 1 -- ^-^ cst (1)
    y = newVar 2 -- ^-^ cst (1)
    z = newVar 3 -- ^-^ cst (1)

cubeConstraints' :: [[Double]]
cubeConstraints' = [[ 1, 0, 0,-1]
                   ,[-1, 0, 0,-1]
                   ,[ 0, 1, 0,-1]
                   ,[ 0,-1, 0,-1]
                   ,[ 0, 0, 1,-1]
                   ,[ 0, 0,-1,-1]]
