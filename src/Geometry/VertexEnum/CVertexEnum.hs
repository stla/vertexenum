{-# LANGUAGE ForeignFunctionInterface #-}
module Geometry.VertexEnum.CVertexEnum
  ( c_intersections )
  where
import Foreign         ( Ptr )
import Foreign.C.Types ( CDouble, CUInt(..) )

foreign import ccall unsafe "intersections" c_intersections
  :: Ptr CDouble            -- halfspaces
  -> Ptr CDouble            -- interior point
  -> CUInt                  -- dim
  -> CUInt                  -- n halfspaces
  -> Ptr CUInt              -- n intersections
  -> Ptr CUInt              -- exitcode
  -> CUInt                  -- 0/1 print to stdout
  -> IO (Ptr (Ptr CDouble))
