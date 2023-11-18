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

data Constraint a = Constraint (LinearCombination a) Sense (LinearCombination a)

instance Show a => Show (Constraint a) where 
  show :: Constraint a -> String
  show (Constraint lhs sense rhs) = show lhs ++ " " ++ show sense ++ " " ++ show rhs 

(.>=.) :: LinearCombination a -> LinearCombination a -> Constraint a
(.>=.) lhs = Constraint lhs Gt

(.<=.) :: LinearCombination a -> LinearCombination a -> Constraint a
(.<=.) lhs = Constraint lhs Lt

(.>=) :: LinearCombination a -> a -> Constraint a
(.>=) lhs x = (.>=.) lhs (constant x)

(.<=) :: LinearCombination a -> a -> Constraint a
(.<=) lhs x = (.<=.) lhs (constant x)

infix 4 .<=., .>=.
