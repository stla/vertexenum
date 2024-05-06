module Geometry.VertexEnum.Internal
  ( normalizeConstraints
  , varsOfConstraint
  , iPoint )
  where
import           Prelude                hiding         ( EQ )
import           Control.Monad.Logger                  (
                                                         runStdoutLoggingT
                                                       , filterLogger
                                                       )
import           Data.IntMap.Strict                    ( IntMap, mergeWithKey )
import qualified Data.IntMap.Strict                    as IM
import           Data.Map.Strict                       ( Map )
import qualified Data.Map.Strict                       as DM
import           Data.Maybe                            ( fromJust )
import           Data.List                             ( nub, union )
import           Data.List.Extra                       ( unsnoc )
import           Geometry.VertexEnum.Constraint        ( Constraint (..), Sense (..) )
import           Geometry.VertexEnum.LinearCombination ( LinearCombination (..), VarIndex )
import           Linear.Simplex.Solver.TwoPhase        (
                                                         twoPhaseSimplex
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
varsOfConstraint (Constraint lhs _ rhs) =
  varsOfLinearCombo lhs `union` varsOfLinearCombo rhs

normalizeConstraint :: Real a => [VarIndex] -> Constraint a -> [a]
normalizeConstraint vars (Constraint lhs sense rhs) =
  if sense == Lt
    then xs ++ [x]
    else map negate xs ++ [-x]
  where
    lhs' = normalizeLinearCombination vars lhs
    rhs' = normalizeLinearCombination vars rhs
    coefs = IM.elems $ mergeWithKey (\_ a b -> Just (a-b)) id id lhs' rhs'
    (x, xs) = case coefs' of
      (xx:xxs) -> (xx, xxs)
      []       -> (0, [])

normalizeConstraints :: Real a => [Constraint a] -> [[a]]
normalizeConstraints constraints = 
  map (normalizeConstraint vars) constraints
  where
    vars = nub $ concatMap varsOfConstraint constraints

inequality :: [Rational] -> PolyConstraint
inequality row = 
  LEQ { 
        lhs = DM.fromList (zip [1 ..] (1 : coeffs)), rhs = -bound 
      } 
  where
    (coeffs, bound) = fromJust $ unsnoc row

inequalities :: [[Rational]] -> [PolyConstraint]
inequalities normConstraints = map inequality normConstraints

iPoint :: [[Rational]] -> IO [Double]
iPoint halfspacesMatrix = do
  maybeResult <- runStdoutLoggingT $ filterLogger (\_ _ -> False) $ 
                  twoPhaseSimplex objFunc polyConstraints
  return $ case maybeResult of
    Just (Result var varLitMap) -> 
      map fromRational 
        (
          DM.elems (DM.delete 1 $ DM.delete var varLitMap) 
        )
    Nothing -> error "failed to find an interior point."
  where
    polyConstraints = inequalities halfspacesMatrix
    objFunc = Max {
        objective = DM.singleton 1 1
      } 
