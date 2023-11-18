module Geometry.VertexEnum.Internal
  ( normalizeConstraints
  , varsOfConstraint
  , iPoint )
  where
import           Data.IntMap.Strict                    ( IntMap, mergeWithKey )
import qualified Data.IntMap.Strict                    as IM
import           Data.List                             ( nub, union )
import           Geometry.VertexEnum.Constraint        ( Constraint (..), Sense (..) )
import           Geometry.VertexEnum.LinearCombination ( LinearCombination (..), VarIndex )
import           Numeric.LinearProgramming             ( simplex,
                                                         Bound(Free, (:<=:)),
                                                         Constraints(Dense),
                                                         Optimization(Maximize),
                                                         Solution(
                                                          Undefined
                                                        , Feasible
                                                        , Infeasible
                                                        , NoFeasible
                                                        , Optimal
                                                        , Unbounded) )

normalizeLinearCombination :: 
  Num a => [VarIndex] -> LinearCombination a -> IntMap a
normalizeLinearCombination vars (LinearCombination lc) =
  IM.union lc (IM.fromList [(i,0) | i <- vars `union` [0]])

varsOfLinearCombo :: LinearCombination a -> [VarIndex]
varsOfLinearCombo (LinearCombination imap) = IM.keys imap

varsOfConstraint :: Constraint a -> [VarIndex]
varsOfConstraint (Constraint lhs _ rhs) =
  varsOfLinearCombo lhs `union` varsOfLinearCombo rhs

normalizeConstraint :: Real a => [VarIndex] -> Constraint a -> [Double]
normalizeConstraint vars (Constraint lhs sense rhs) =
  if sense == Lt
    then xs ++ [x]
    else map negate xs ++ [-x]
  where
    lhs' = normalizeLinearCombination vars lhs
    rhs' = normalizeLinearCombination vars rhs
    coefs = IM.elems $ mergeWithKey (\_ a b -> Just (a-b)) id id lhs' rhs'
    coefs' :: [Double]
    coefs' = map realToFrac coefs
    (x, xs) = case coefs' of
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

normalizeConstraints :: Real a => [Constraint a] -> [[Double]] -- for qhalf
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

iPoint :: [[Double]] -> [Double]
iPoint halfspacesMatrix = case solution of
  Optimal (_, point) -> init point
  Undefined          -> error "Failed to find interior point (undefined)."
  Feasible (_, _)    -> error "Failed to find interior point (feasible)."
  Infeasible (_, _)  -> error "Failed to find interior point (infeasible)."
  NoFeasible         -> error "Failed to find interior point (no feasible)."
  Unbounded          -> error "Failed to find interior point (unbounded)."  
  where
    constraints' = inequalities halfspacesMatrix
    n = length (head halfspacesMatrix)
    objective = Maximize (replicate (n-1) 0 ++ [1])
    bounds = map Free [1 .. (n-1)]
    solution = simplex objective constraints' bounds
