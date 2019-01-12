{-# LANGUAGE FlexibleInstances #-}
module GI.Cairo.Structs.RectangleInt where

import           Data.GI.Base.BasicTypes
import           Data.GI.Base.ManagedPtr
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified Graphics.Cairo.Types as T
import           Helper

newtype RectangleInt = RectangleInt (ManagedPtr RectangleInt)

instance BoxedObject RectangleInt where
  boxedType _ = rectangle_intGType

foreign import ccall safe "cairo_gobject_rectangle_int_get_type"
  rectangle_intGType :: IO GType

instance Connector RectangleInt (T.Rectangle Int) where
  fromGI (RectangleInt (ManagedPtr fp _ _)) = withForeignPtr (castForeignPtr fp) peek
  toGI ri = do
    p <- malloc
    poke p ri
    RectangleInt <$> newManagedPtr_ (castPtr p)
