module Geometry.VertexEnum.Internal
  ( normalizeConstraints
  , varsOfConstraint
  , interiorPoint )
  where
import           Data.IntMap.Strict                    ( IntMap, mergeWithKey )
import qualified Data.IntMap.Strict                    as IM
import           Data.List                             ( nub, union )
import           Data.Ratio                            ( (%), numerator, denominator )
import           Geometry.VertexEnum.Constraint        ( Constraint (..), Sense (..) )
import           Geometry.VertexEnum.LinearCombination ( LinearCombination (..), VarIndex )
import           Numeric.LinearProgramming             ( simplex,
                                                         Bound(Free, (:<=:)),
                                                         Constraints(Dense),
                                                         Optimization(Maximize),
                                                         Solution(Optimal) )


normalizeLinearCombination :: 
  [VarIndex] -> LinearCombination -> IntMap Rational
normalizeLinearCombination vars (LinearCombination lc) =
  IM.union lc (IM.fromList [(i,0) | i <- vars `union` [0]])

varsOfLinearCombo :: LinearCombination -> [VarIndex]
varsOfLinearCombo (LinearCombination imap) = IM.keys imap

varsOfConstraint :: Constraint -> [VarIndex]
varsOfConstraint (Constraint lhs _ rhs) =
  varsOfLinearCombo lhs `union` varsOfLinearCombo rhs

normalizeConstraint :: [VarIndex] -> Constraint -> [Double]
normalizeConstraint vars (Constraint lhs sense rhs) =
  if sense == Lt
    then xs ++ [x]
    else map negate xs ++ [-x]
  where
    lhs' = normalizeLinearCombination vars lhs
    rhs' = normalizeLinearCombination vars rhs
    coefs = IM.elems $ mergeWithKey (\_ a b -> Just (a-b)) id id lhs' rhs'
    denominators = map denominator coefs
    ppcm = foldr lcm 1 denominators % 1
    (x, xs) = case map (realToFrac . numerator . (*ppcm)) coefs of
      (xx:xxs)  -> (xx, xxs)
      [] -> (0, [])
  -- let (x:xs) = map realToFrac $
  --              IM.elems $ mergeWithKey (\_ a b -> Just (a-b)) id id lhs' rhs'
  -- in
  -- if sense == Lt
  --   then xs ++ [x]
  --   else map negate xs ++ [-x]
  -- where lhs' = normalizeLinearCombination vars lhs
  --       rhs' = normalizeLinearCombination vars rhs

normalizeConstraints :: [Constraint] -> [[Double]] -- for qhalf
normalizeConstraints constraints = 
  map (normalizeConstraint vars) constraints
  where
    vars = nub $ concatMap varsOfConstraint constraints

inequality :: [Double] -> Bound [Double]
inequality row = (coeffs ++ [1.0]) :<=: bound
  where
    coeffs = init row
    bound = -(last row)

inequalities :: [[Double]] -> Constraints
inequalities normConstraints = Dense (map inequality normConstraints)

interiorPoint :: [[Double]] -> [Double]
interiorPoint halfspacesMatrix = case solution of
  Optimal (_, point) -> init point
  _                  -> error "Failed to find interior point."
  where
    constraints' = inequalities halfspacesMatrix
    n = length (head halfspacesMatrix)
    objective = Maximize (replicate (n-1) 0 ++ [1])
    bounds = map Free [1 .. (n-1)]
    solution = simplex objective constraints' bounds

