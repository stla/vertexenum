module Geometry.VertexEnum.Internal
  ( normalizeConstraints
  , varsOfConstraint
  , feasiblePoint
  , findSigns
  , iPoint )
  where
import           Prelude                hiding         ( EQ )
import           Control.Monad.Logger                  (
                                                         runStdoutLoggingT
                                                       , filterLogger
                                                       )
import           Data.IntMap.Strict                    ( IntMap, mergeWithKey )
import qualified Data.IntMap.Strict                    as IM
import qualified Data.Map.Strict                       as DM
import           Data.Maybe                            ( fromJust, isJust )
import           Data.List                             ( nub, union )
import           Data.List.Extra                       ( unsnoc )
import           Geometry.VertexEnum.Constraint        ( Constraint (..), Sense (..) )
import           Geometry.VertexEnum.LinearCombination ( LinearCombination (..), VarIndex )
import           Linear.Simplex.Solver.TwoPhase        (
                                                         twoPhaseSimplex
                                                       , findFeasibleSolution
                                                       )
import           Linear.Simplex.Types                  (
                                                         Result ( .. )
                                                       , PolyConstraint ( .. )
                                                       , ObjectiveFunction ( .. )
                                                       )
import           Linear.Simplex.Util                   (
                                                         simplifySystem
                                                       )

normalizeLinearCombination :: 
  Num a => [VarIndex] -> LinearCombination a -> IntMap a
normalizeLinearCombination vars (LinearCombination lc) =
  IM.union lc (IM.fromList [(i,0) | i <- vars `union` [0]])

varsOfLinearCombo :: LinearCombination a -> [VarIndex]
varsOfLinearCombo (LinearCombination imap) = IM.keys imap

varsOfConstraint :: Constraint a -> [VarIndex]
varsOfConstraint (Constraint left _ right) =
  varsOfLinearCombo left `union` varsOfLinearCombo right

normalizeConstraint :: Real a => [VarIndex] -> Constraint a -> [a]
normalizeConstraint vars (Constraint left sense right) =
  if sense == Lt
    then xs ++ [x]
    else map negate xs ++ [-x]
  where
    lhs' = normalizeLinearCombination vars left
    rhs' = normalizeLinearCombination vars right
    coefs = IM.elems $ mergeWithKey (\_ a b -> Just (a-b)) id id lhs' rhs'
    (x, xs) = case coefs of
      (xx:xxs) -> (xx, xxs)
      []       -> (0, [])

normalizeConstraints :: Real a => [Constraint a] -> [[a]]
normalizeConstraints constraints = 
  map (normalizeConstraint vars) constraints
  where
    vars = nub $ concatMap varsOfConstraint constraints

negateIf :: Bool -> Rational -> Rational
negateIf test x = if test then -x else x

inequality :: [Bool] -> [Rational] -> PolyConstraint
inequality toNegate row = 
  LEQ { 
        lhs = DM.fromList (zip [0 ..] (1 : coeffs')), rhs = -bound
      }
  where
    (coeffs, bound) = fromJust $ unsnoc row
    coeffs' = zipWith negateIf toNegate coeffs

inequalities :: [[Rational]] -> [Bool] -> [PolyConstraint]
inequalities normConstraints toNegate = 
  simplifySystem $ map (inequality toNegate) normConstraints

-- iPoint does not necessarily return the optimal interior point, because 
-- this point possibly corresponds to another [Bool] combination; it just 
-- returns a feasible point
iPoint :: [[Rational]] -> [Bool] -> IO [Double]
iPoint halfspacesMatrix toNegate = do
  maybeResult <- runStdoutLoggingT $ filterLogger (\_ _ -> False) $ 
                  twoPhaseSimplex objFunc polyConstraints
  return $ case maybeResult of
    Just (Result var varLitMap) -> 
      let sol = DM.delete 0 $ DM.delete var varLitMap
          nvars = length toNegate
          sol' = DM.union sol (DM.fromList (zip [1 .. nvars] (repeat 0)))
      in 
      map fromRational 
        (
          zipWith negateIf toNegate (DM.elems sol')
        )
    Nothing -> error "iPoint: should not happen."
  where
    polyConstraints = inequalities halfspacesMatrix toNegate
    objFunc = Max {
        objective = DM.singleton 0 1
      } 

feasiblePoint :: [[Rational]] -> [Bool] -> IO Bool
feasiblePoint halfspacesMatrix toNegate = do
  maybeFS <- runStdoutLoggingT $ filterLogger (\_ _ -> False) $ 
                  findFeasibleSolution polyConstraints
  return $ isJust maybeFS
  where
    polyConstraints = simplifySystem $ map ineq halfspacesMatrix
    ineq row = 
      LEQ { 
            lhs = DM.fromList (zip [1 ..] coeffs'), rhs = -bound
          } 
      where
        (coeffs, bound) = fromJust $ unsnoc row
        coeffs' = zipWith negateIf toNegate coeffs

findSigns :: [[Rational]] -> IO [Bool]
findSigns halfspacesMatrix = do 
  go 0
  where
    nvars = length (halfspacesMatrix !! 0) - 1
    combinations = sequence $ replicate nvars [False, True]
    ncombinations = length combinations
    go i 
      | i == ncombinations = do 
          return []
      | otherwise          = do 
          let combo = combinations !! i
          test <- feasiblePoint halfspacesMatrix combo
          if test 
            then do
              return $ combo
            else do
              go (i+1)
