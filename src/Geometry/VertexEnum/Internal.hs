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

inequality :: [Bool] -> [Rational] -> PolyConstraint
inequality toNegate row = 
  LEQ { 
        lhs = DM.filter (/= 0) (DM.fromList (zip [1 ..] (1 : coeffs))), rhs = -bound
      }
  where
    (coeffs0, bound) = fromJust $ unsnoc row
    coeffs = [if toNegate !! i then -coeffs0 !! i else coeffs0 !! i | i <- [0 .. length coeffs0 - 1]]

inequalities :: [[Rational]] -> [Bool] -> [PolyConstraint]
inequalities normConstraints toNegate = 
  -- EQ { 
  --       lhs = DM.singleton 0 1, rhs = 1
  --     }
  [LEQ { lhs = DM.fromList [(1, 1), (i, -1)], rhs = 0 } | i <- [2 .. nvars + 1]]
   ++ map (inequality toNegate) normConstraints
  where 
    nvars = length toNegate

iPoint :: [[Rational]] -> [Bool] -> IO [Double]
iPoint halfspacesMatrix toNegate = do
  maybeResult <- runStdoutLoggingT $ filterLogger (\_ _ -> False) $ 
                  twoPhaseSimplex objFunc polyConstraints
  fs <- runStdoutLoggingT $ filterLogger (\_ _ -> False) $ 
                findFeasibleSolution polyConstraints
  print halfspacesMatrix
  print polyConstraints
  print maybeResult
  print fs
  return $ case maybeResult of
    Just (Result var varLitMap) -> 
      map fromRational 
        (
          map (negateIf) (zip toNegate (DM.elems (DM.delete 1 $ DM.delete var varLitMap))) 
        )
    Nothing -> error "failed to find an interior point."
  where
    negateIf (test, x) = if test then -x else x
    polyConstraints = inequalities halfspacesMatrix toNegate
    objFunc = Max {
        objective = DM.singleton 1 1
      } 

feasiblePoint :: [[Rational]] -> [Bool] -> IO Bool
feasiblePoint halfspacesMatrix toNegate = do
  maybeFS <- runStdoutLoggingT $ filterLogger (\_ _ -> False) $ 
                  findFeasibleSolution polyConstraints
  return $ isJust maybeFS
  where
    polyConstraints = map ineq halfspacesMatrix
    ineq row = 
      LEQ { 
            lhs = DM.fromList (zip [1 ..] coeffs'), rhs = -bound 
          } 
      where
        (coeffs, bound) = fromJust $ unsnoc row
        coeffs' = [if toNegate !! i then -coeffs !! i else coeffs !! i | i <- [0 .. length coeffs - 1]]

findSigns :: [[Rational]] -> IO [Bool]
findSigns halfspacesMatrix = do 
  go 0
  where
    nvars = length (halfspacesMatrix !! 0) - 1
    combinations = sequence $ replicate nvars [False, True]
    ncombinations = length combinations
    go i 
      | i == ncombinations = error "XXXXXXXXXXX"
      | otherwise = do 
          let combo = combinations !! i
          test <- feasiblePoint halfspacesMatrix combo
          if test 
            then do
              return $ combo
            else do
              go (i+1)


-- makeFeasibleSystem :: [Constraint Rational] -> [Bool] -> FeasibleSystem
-- makeFeasibleSystem constraints toNegate = FeasibleSystem dico slackvars [] ovar
--   where
--     halfspacesMatrix = normalizeConstraints constraints
--     nconstraints = length halfspacesMatrix
--     nvars = length (halfspacesMatrix !! 0) 
--     d (i, row) = (i, dictvalue)
--       where 
--         row' = map negate (1 : row)
--         coeffs0 = init row'
--         coeffs = [if toNegate !! i then -coeffs0 !! i else coeffs0 !! i | i <- [0 .. length coeffs0 - 1]]
--         bound = last row'
--         dictvalue = DictValue {varMapSum = DM.fromList (zip [1..] coeffs), constant = bound}
--     dico = DM.fromList (map d (zip [nvars+1 ..] halfspacesMatrix))
--     slackvars = [nconstraints + nvars, nconstraints+nvars-1 .. nvars+1]
--     ovar = nconstraints + nvars + 1

-- optimize :: [Constraint Rational] -> [Bool] -> IO (Maybe Result)
-- optimize constraints toNegate = do
--   runStdoutLoggingT $ filterLogger (\_ _ -> False) $ 
--                   optimizeFeasibleSystem objFunc fs
--   where
--     fs = makeFeasibleSystem constraints toNegate
--     objFunc = Max {
--         objective = DM.singleton 1 1
--       } 


