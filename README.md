# vertexenum

*Get the vertice of halfspaces intersection.*

____

Consider the following system of linear inequalities:

$$\left\{\begin{matrix} -5 & \leqslant & x & \leqslant & 4 \\ -5 & \leqslant & y & \leqslant & 3-x \\ -10 & \leqslant & z & \leqslant & 6-2x-y \end{matrix}\right..$$

Each inequality defines a halfspace. The intersection of the six halfspaces is
a convex polytope. The `vertexenum` function can calculate the vertices of this 
polytope:

```haskell
import Data.Ratio           ( (%) )
import Data.VectorSpace     ( AdditiveGroup((^+^), (^-^))
                            , VectorSpace((*^)) )
import Geometry.VertexEnum

constraints :: [Constraint]
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