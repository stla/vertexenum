{-# LANGUAGE InstanceSigs #-}
module Geometry.VertexEnum.Constraint
  ( Sense (..)
  , Constraint (..)
  , (.>=.)
  , (.<=.)
  , (.>=)
  , (.<=) )
  where
import Geometry.VertexEnum.LinearCombination ( LinearCombination, constant )

data Sense = Gt | Lt
  deriving Eq

instance Show Sense where
  show :: Sense -> String
  show Gt = ">="
  show Lt = "<="

data Constraint = Constraint LinearCombination Sense LinearCombination
  deriving (Eq, Show)

(.>=.) :: LinearCombination -> LinearCombination -> Constraint
(.>=.) lhs rhs = Constraint lhs Gt rhs

(.<=.) :: LinearCombination -> LinearCombination -> Constraint
(.<=.) lhs rhs = Constraint lhs Lt rhs

(.>=) :: LinearCombination -> Rational -> Constraint
(.>=) lhs x = (.>=.) lhs (constant x)

(.<=) :: LinearCombination -> Rational -> Constraint
(.<=) lhs x = (.<=.) lhs (constant x)

infix 4 .<=., .>=.
