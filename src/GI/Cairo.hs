module GI.Cairo (Render, module Graphics.Cairo.Render, module GI.Cairo.Enums) where

import           GI.Cairo.Enums
import           Graphics.Cairo.Render hiding (Render)
import qualified Graphics.Cairo.Render as R (Render)

type Render a = R.Render IO a
