{-|
Module      : Geometry.VertexEnum
Description : Vertex enumeration of convex polytopes.
Copyright   : (c) Stéphane Laurent, 2023
License     : GPL-3
Maintainer  : laurent_step@outlook.fr

See README for an example.
-}
module Geometry.VertexEnum
  ( module X )
  where
import Geometry.VertexEnum.Constraint        as X ( Constraint(..)
                                                  , Sense(..)
                                                  , (.>=.), (.<=.), (.>=), (.<=) 
                                                  )
import Geometry.VertexEnum.LinearCombination as X ( VarIndex
                                                  , LinearCombination(..)
                                                  , newVar
                                                  , linearCombination
                                                  , constant
                                                  , cst )
import Geometry.VertexEnum.VertexEnum        as X ( vertexenum 
                                                  , checkConstraints
                                                  , interiorPoint )
