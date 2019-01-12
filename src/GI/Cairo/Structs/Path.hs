module GI.Cairo.Structs.Path where

import           Data.GI.Base.BasicTypes
import           Data.GI.Base.ManagedPtr
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified Graphics.Cairo.Types as T
import           Helper

newtype Path = Path (ManagedPtr Path)

instance BoxedObject Path where
  boxedType _ = pathGType

foreign import ccall safe "cairo_gobject_path__get_type"
  pathGType :: IO GType

instance Connector Path T.Path where
  fromGI (Path (ManagedPtr fp _ _)) = withForeignPtr (castForeignPtr fp) peek
  toGI path = do
    p <- malloc
    poke p path
    Path <$> newManagedPtr_ (castPtr p)
