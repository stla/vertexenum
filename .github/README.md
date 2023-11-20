# vertexenum

<!-- badges: start -->
[![Stack](https://github.com/stla/vertexenum/actions/workflows/Stack.yml/badge.svg)](https://github.com/stla/vertexenum/actions/workflows/Stack.yml)
<!-- badges: end -->

*Get the vertices of an intersection of halfspaces.*

____

This package depends on the packages **hmatrix** and **hmatrix-glpk**; follow 
[this link](https://github.com/haskell-numerics/hmatrix/blob/master/INSTALL.md) 
for installation instructions.

Consider the following system of linear inequalities:

$$\left\{\begin{matrix} -5 & \leqslant & x & \leqslant & 4 \\\ -5 & \leqslant & y & \leqslant & 3-x \\\ -10 & \leqslant & z & \leqslant & 6-2x-y \end{matrix}.\right.$$

Each inequality defines a halfspace. The intersection of the six halfspaces is
a convex polytope. The `vertexenum` function can calculate the vertices of this 
polytope:

```haskell
import Data.VectorSpace     ( AdditiveGroup((^+^), (^-^))
                            , VectorSpace((*^)) )
import Geometry.VertexEnum

constraints :: [Constraint Double]
constraints =
  [ x .>= (-5)         -- shortcut for `x .>=. cst (-5)`
  , x .<=  4
  , y .>= (-5)
  , y .<=. cst 3 ^-^ x -- we need `cst` here
  , z .>= (-10)
  , z .<=. cst 6 ^-^ 2*^x ^-^ y ]
  where
    x = newVar 1
    y = newVar 2
    z = newVar 3

vertexenum constraints Nothing
```

The type of the second argument of `vertexenum` is `Maybe [Double]`. If this 
argument is `Just point`, then `point` must be the coordinates of a point 
interior to the polytope. If this argument is `Nothing`, an interior point 
is automatically calculated. You can get it with the `interiorPoint` function. 
It is easy to mentally get an interior point for the above example, but in 
general this is not an easy problem.