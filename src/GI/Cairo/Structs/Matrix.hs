{-# LANGUAGE FlexibleInstances #-}
module GI.Cairo.Structs.Matrix where

import           Data.GI.Base.BasicTypes
import           Data.GI.Base.ManagedPtr
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified Graphics.Cairo.Types as T
import           Helper

newtype Matrix = Matrix (ManagedPtr Matrix)

instance BoxedObject Matrix where
  boxedType _ = matrixGType

foreign import ccall safe "cairo_gobject_matrix__get_type"
  matrixGType :: IO GType

instance Connector Matrix (T.Matrix Double) where
  fromGI (Matrix (ManagedPtr fp _ _)) = withForeignPtr (castForeignPtr fp) peek
  toGI md = do
    p <- malloc
    poke p md
    Matrix <$> newManagedPtr_ (castPtr p)
