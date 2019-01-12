module GI.Cairo.Enums (Antialias(..), Content(..), Extend(..), FillRule(..), Filter(..), FontSlant(..), FontWeight(..), Format(..), HintMetrics(..), HintStyle(..), LineCap(..), LineJoin(..), Operator(..), PathDataType(..), SubpixelOrder(..), Status(..), FontType(..), PatternType(..), SurfaceType(..), TextClusterFlags(..), DeviceType(..), RegionOverlap(..)) where

import Data.GI.Base.BasicTypes
import Graphics.Cairo.Types
import Helper

$(concat <$> mapM genForEnum [''Antialias, ''Content, ''Extend, ''FillRule, ''Filter, ''FontSlant, ''FontWeight, ''Format, ''HintMetrics, ''HintStyle, ''LineCap, ''LineJoin, ''Operator, ''PathDataType, ''SubpixelOrder, ''Status, ''FontType, ''PatternType, ''SurfaceType, ''TextClusterFlags, ''DeviceType, ''RegionOverlap])
