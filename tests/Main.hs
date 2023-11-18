module Main where
import Approx                                ( assertApproxZero )
import Geometry.VertexEnum.Constraint        ( (.<=), (.>=), Constraint )
import Geometry.VertexEnum.LinearCombination ( newVar )
import Geometry.VertexEnum.VertexEnum        ( vertexenum, interiorPoint )    
import Test.Tasty                            ( defaultMain, testGroup )
import Test.Tasty.HUnit                      ( testCase, assertEqual )

cubeConstraints :: [Constraint]
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

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ 

    testCase "cube vertices" $ do
      vertices <- vertexenum cubeConstraints (Just [0, 0, 0])
      assertEqual "" (length vertices) 8

  , testCase "interior point of the cube" $ do
      let ipoint = interiorPoint cubeConstraints
          norm = (ipoint !! 0) * (ipoint !! 0) + 
                 (ipoint !! 1) * (ipoint !! 1) + (ipoint !! 2) * (ipoint !! 2)
      assertApproxZero "" 6 norm 

  ]
