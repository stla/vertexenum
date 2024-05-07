module Geometry.VertexEnum.VertexEnum
  ( vertexenum
  , checkConstraints
  , interiorPoint )
  where
import           Control.Monad                   ( unless, when, (<$!>) )
import           Data.Maybe                      ( isJust, fromJust )
import           Foreign.C.Types                 ( CDouble, CUInt )
import           Foreign.Marshal.Alloc           ( free, mallocBytes )
import           Foreign.Marshal.Array           ( peekArray, pokeArray )
import           Foreign.Storable                ( peek, sizeOf )
import           Geometry.VertexEnum.CVertexEnum ( c_intersections )
import           Geometry.VertexEnum.Constraint  ( Constraint, toRationalConstraint )
import           Geometry.VertexEnum.Internal    ( iPoint, normalizeConstraints, findSigns )

hsintersections :: [[Double]]     -- halfspaces
                 -> [Double]      -- interior point
                 -> Bool          -- print to stdout
                 -> IO [[Double]]
hsintersections halfspaces ipoint stdout = do
  let n     = length halfspaces
      dim   = length ipoint
  unless (all ((== dim+1) . length) halfspaces) $
    error "the points must have the same dimension"
  when (dim < 2) $
    error "dimension must be at least 2"
  when (n <= dim) $
    error "insufficient number of halfspaces"
  hsPtr <- mallocBytes (n * (dim+1) * sizeOf (undefined :: CDouble))
  pokeArray hsPtr (concatMap (map realToFrac) halfspaces)
  ipointPtr <- mallocBytes (dim * sizeOf (undefined :: CDouble))
  pokeArray ipointPtr (map realToFrac ipoint)
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  nintersectionsPtr <- mallocBytes (sizeOf (undefined :: CUInt))
  resultPtr <- c_intersections hsPtr ipointPtr
               (fromIntegral dim) (fromIntegral n)
               nintersectionsPtr exitcodePtr (fromIntegral $ fromEnum stdout)
  exitcode <- peek exitcodePtr
  free exitcodePtr
  free hsPtr
  if exitcode /= 0
    then do
      free resultPtr
      free nintersectionsPtr
      error $ "qhull returned an error (code " ++ show exitcode ++ ")"
    else do
      nintersections <- (<$!>) fromIntegral (peek nintersectionsPtr)
      result <- (<$!>) (map (map realToFrac))
                       ((=<<) (mapM (peekArray dim))
                              (peekArray nintersections resultPtr))
      free resultPtr
      free nintersectionsPtr
      return result

-- | Vertex enumeration
vertexenum :: Real a => [Constraint a]   -- ^ list of inequalities
           -> Maybe [Double] -- ^ point in the interior of the polytope
           -> IO [[Double]]
vertexenum constraints point = do
  let halfspacesMatrix = 
        map (map realToFrac) (normalizeConstraints constraints)
  if isJust point
    then do
      hsintersections halfspacesMatrix (fromJust point) False
    else do
      let halfspacesMatrix' =
            map (map toRational) (normalizeConstraints constraints)
      ipoint <- iPoint halfspacesMatrix' [False]
      hsintersections halfspacesMatrix ipoint False

-- | Check whether a point fulfills some constraints; returns the 
-- difference between the upper member and the lower member for each
-- constraint, which is positive in case if the constraint is fulfilled
checkConstraints :: Real a => [Constraint a]     -- ^ list of inequalities
                 -> [Double]         -- ^ point to be tested
                 -> [(Double, Bool)] -- ^ difference and status for each constraint
checkConstraints constraints point = 
  if nvars == length point + 1
    then 
      zip differences (map (>= 0) differences)
    else 
      error "checkConstraints: the length of the point does not match the number of variables."
  where
    halfspacesMatrix = 
      map (map realToFrac) (normalizeConstraints constraints)
    nvars = length (halfspacesMatrix !! 0)
    checkRow pt row = - sum (zipWith (*) row (pt ++ [1]))
    differences = map (checkRow point) halfspacesMatrix

-- | Returns a point fulfilling a list of constraints
interiorPoint :: Real a => [Constraint a] -> IO [Double]
interiorPoint constraints = do 
  let
    constraints' = map toRationalConstraint constraints
    halfspacesMatrix = normalizeConstraints constraints'
  signs <- findSigns constraints'
  iPoint halfspacesMatrix signs
