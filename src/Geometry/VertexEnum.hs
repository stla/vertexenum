{-|
Module      : Geometry.VertexEnum
Description : Vertex enumeration of convex polytopes.
Copyright   : (c) Stéphane Laurent, 2023-2024
License     : GPL-3
Maintainer  : laurent_step@outlook.fr

Enumeration of the vertices of a convex polytope given by linear 
inequalities. See README for an example.
-}
module Geometry.VertexEnum
  ( module X )
  where
import Geometry.VertexEnum.Constraint        as X ( Constraint (..)
                                                  , Sense (..)
                                                  , (.>=.), (.<=.), (.>=), (.<=) 
                                                  )
import Geometry.VertexEnum.LinearCombination as X ( VarIndex
                                                  , LinearCombination (..)
                                                  , newVar
                                                  , linearCombination
                                                  , constant
                                                  , cst )
import Geometry.VertexEnum.VertexEnum        as X ( vertexenum 
                                                  , checkConstraints
                                                  , interiorPoint )
import Geometry.VertexEnum.Internal        as X ( feasiblePoint, makeFeasibleSystem, optimize, iPoint )
