{-# LANGUAGE OverloadedLabels, OverloadedStrings, UnicodeSyntax #-}

-- original author:
--    Mirco "MacSlow" Mueller <macslow@bangang.de>
--
-- created:
--    10.1.2006 (or so)
--
-- http://www.gnu.org/licenses/licenses.html#GPL
--
-- ported to Haskell (gtk2hs) by:
--    Duncan Coutts <duncan.coutts@worc.ox.ac.uk>
--
-- ported to haskell-gi from the gtk2hs port by:
--    Iñaki García Etxebarria <garetxe@gmail.com>
--

import           Control.Monad                   ( when )
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe                      ( isJust )
import           System.Time

import           Data.GI.Base

import           GI.Cairo.Connector
import           GI.Cairo.Enums
import qualified GI.Gdk                          as Gdk
import qualified GI.GdkPixbuf                    as GP
import qualified GI.GLib                         as GLib
import qualified GI.Gtk                          as Gtk
import           Graphics.Cairo.Drawing.Cairo    ( create )
import           Graphics.Cairo.Drawing.Patterns
import           Graphics.Cairo.HasStatus
import           Graphics.Cairo.Render
import           Graphics.Cairo.Surfaces.Image
import           Graphics.Cairo.Surfaces.Surface

renderWith s r = do
  c <- create s
  use c $ \c' -> runRender c' r

withRadialPattern a b c d e f g = do
  p <- patternCreateRadial a b c d e f
  with p g

withLinearPattern a b c d e = do
  p <- patternCreateLinear a b c d
  with p e

drawClockBackground quality width height = do
  save
  scale (fromIntegral width) (fromIntegral height)

  save
  setOperator OperatorOver
  when quality drawDropShadow
  drawClockFace quality
  restore

  translate 0.5 0.5
  scale 0.4 0.4
  setSourceRgb 0.16 0.18 0.19
  setLineWidth (1.5/60)
  setLineCap LineCapRound
  setLineJoin LineJoinRound
  drawHourMarks

  restore

drawClockHands ∷ Bool → Int → Int → Render IO ()
drawClockHands quality width height = do
  save
  scale (fromIntegral width) (fromIntegral height)

  translate 0.5 0.5
  scale 0.4 0.4
  setSourceRgb 0.16 0.18 0.19
  setLineWidth (1.5/60)
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  time <- liftIO (getClockTime >>= toCalendarTime)
  let hours   = fromIntegral (if ctHour time >= 12
                                then ctHour time - 12
                                else ctHour time)
      minutes = fromIntegral (ctMin time)
      seconds = fromIntegral (ctSec time)

  drawHourHand quality hours minutes seconds
  drawMinuteHand quality minutes seconds
  drawSecondHand quality seconds

  restore

drawClockForeground ∷ Bool → Int → Int → Render IO ()
drawClockForeground quality width height = do
  scale (fromIntegral width) (fromIntegral height)

  save
  translate 0.5 0.5
  scale 0.4 0.4
  setSourceRgb 0.16 0.18 0.19
  setLineWidth (1.5/60)
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  when quality drawInnerShadow
  when quality drawReflection
  drawFrame quality
  restore

drawDropShadow =
  withRadialPattern 0.55 0.55 0.25 0.5 0.5 0.525 $ \pattern -> do
    patternAddColorStopRgba pattern 0    0     0     0     0.811
    patternAddColorStopRgba pattern 0.64 0.345 0.345 0.345 0.317
    patternAddColorStopRgba pattern 0.84 0.713 0.713 0.713 0.137
    patternAddColorStopRgba pattern 1    1     1     1     0
    patternSetFilter pattern FilterFast
    setSource pattern
    arc 0.5 0.5 (142/150) 0 (pi*2)
    fill

drawClockFace ∷ Bool → Render IO ()
drawClockFace True =
  withLinearPattern 0.5 0 0.5 1 $ \pattern -> do
    patternAddColorStopRgb pattern 0 0.91 0.96 0.93
    patternAddColorStopRgb pattern 1 0.65 0.68 0.68
    patternSetFilter pattern FilterFast
    setSource pattern
    translate 0.5 0.5
    arc 0 0 (60/150) 0 (pi*2)
    fill
drawClockFace False = do
  setSourceRgb 0.78 0.82 0.805
  translate 0.5 0.5
  arc 0 0 (60/150) 0 (pi*2)
  fill

drawHourMarks = do
  save
  forM_ ([1..12] :: [Integer]) $ \_ -> do
    rotate (pi/6)
    moveTo (4.5/6) 0
    lineTo (5.0/6) 0
  stroke
  restore

forM_ = flip mapM_

drawHourHand quality hours minutes seconds = do
  save
  rotate (-pi/2)
  setLineCap LineCapSquare
  setLineJoin LineJoinMiter
  rotate ( (pi/6) * hours
         + (pi/360) * minutes
         + (pi/21600) * seconds)

  -- hour hand's shadow
  when quality $ do
    setLineWidth (1.75/60)
    setOperator OperatorAtop
    setSourceRgba 0.16 0.18 0.19 0.125
    moveTo (-2/15 + 0.025) 0.025
    lineTo (7/15 + 0.025) 0.025
    stroke

  -- the hand itself
  setLineWidth (1/60)
  setOperator OperatorOver
  setSourceRgb 0.16 0.18 0.19
  moveTo (-2/15) 0
  lineTo (7/15) 0
  stroke
  restore

drawMinuteHand quality minutes seconds = do
  save
  rotate (-pi/2)
  setLineCap LineCapSquare
  setLineJoin LineJoinMiter
  rotate ( (pi/30) * minutes
         + (pi/1800) * seconds)

  -- minute hand's shadow
  when quality $ do
    setLineWidth (1.75/60)
    setOperator OperatorAtop
    setSourceRgba 0.16 0.18 0.19 0.125
    moveTo (-16/75 - 0.025) (-0.025)
    lineTo (2/3 - 0.025)    (-0.025)
    stroke

  -- the minute hand itself
  setLineWidth (1/60)
  setOperator OperatorOver
  setSourceRgb 0.16 0.18 0.19
  moveTo (-16/75) 0
  lineTo (2/3) 0
  stroke
  restore

drawSecondHand quality seconds = do
  save
  rotate (-pi/2)
  setLineCap LineCapSquare
  setLineJoin LineJoinMiter
  rotate (seconds * pi/30);

  -- shadow of second hand-part
  when quality $ do
    setOperator  OperatorAtop
    setSourceRgba 0.16 0.18 0.19 0.125
    setLineWidth  (1.3125 / 60)
    moveTo (-1.5/5 + 0.025) 0.025
    lineTo (3/5 + 0.025) 0.025
    stroke

  -- second hand
  setOperator OperatorOver
  setSourceRgb 0.39 0.58 0.77
  setLineWidth (0.75/60)
  moveTo (-1.5/5) 0
  lineTo (3/5) 0
  stroke

  arc 0 0 (1/20) 0 (pi*2)
  fill
  arc (63/100) 0 (1/35) 0 (pi*2)
  stroke
  setLineWidth  (1/100)
  moveTo  (10/15) 0
  lineTo  (12/15) 0
  stroke
  setSourceRgb  0.31 0.31 0.31
  arc  0 0 (1/25) 0 (pi*2)
  fill
  restore

drawInnerShadow = do
  save
  setOperator OperatorOver
  arc 0 0 (142/150) 0 (pi*2)
  clip
  withRadialPattern 0.3 0.3 0.1 0 0 0.95 $ \pattern -> do
    patternAddColorStopRgba pattern 0    1     1     1     0
    patternAddColorStopRgba pattern 0.64 0.713 0.713 0.713 0.137
    patternAddColorStopRgba pattern 0.84 0.345 0.345 0.345 0.317
    patternAddColorStopRgba pattern 1    0     0     0     0.811
    patternSetFilter pattern FilterFast
    setSource pattern
    arc 0 0 (142/150) 0 (pi*2)
    fill
  restore

drawReflection = do
  save
  arc 0 0 (142/150) 0 (pi*2)
  clip
  rotate (-75 * pi/180)
  setSourceRgba 0.87 0.9 0.95 0.25
  moveTo (-1) (-1)
  lineTo 1 (-1)
  lineTo 1 1
  curveTo 1 0.15 (-0.15) (-1) (-1) (-1)
  fill
  moveTo (-1) (-1)
  lineTo (-1) 1
  lineTo 1 1
  curveTo (-0.5) 1 (-1) 0.5 (-1) (-1)
  fill
  restore

drawFrame ∷ Bool → Render IO ()
drawFrame True = do
  save
  withRadialPattern (-0.1) (-0.1) 0.8 0 0 1.5 $ \pattern -> do
    patternAddColorStopRgb pattern 0   0.4  0.4  0.4
    patternAddColorStopRgb pattern 0.2 0.95 0.95 0.95
    patternSetFilter pattern FilterFast
    setSource pattern
    setLineWidth (10/75)
    arc 0 0 (142/150) 0 (pi*2)
    stroke

  withRadialPattern (-0.1) (-0.1) 0.8 0 0 1.5 $ \pattern -> do
    patternAddColorStopRgb pattern 0   0.9  0.9  0.9
    patternAddColorStopRgb pattern 0.2 0.35 0.35 0.35
    patternSetFilter pattern FilterFast
    setSource pattern
    setLineWidth (10/75)
    arc 0 0 (150/150) 0 (pi*2)
    stroke
  restore
drawFrame False = do
  save
  setSourceRgb 0 0 0
  setLineWidth (10/75)
  arc 0 0 1 0 (pi*2)
  stroke
  restore

initialSize ∷ Int
initialSize = 256

main ∷ IO ()
main = do
  _ <- Gtk.init Nothing

  window <- new Gtk.Window
            [ #decorated      := False
            , #resizable      := True
            , #windowPosition := Gtk.WindowPositionCenterAlways
            , #appPaintable   := True
            , #icon           :=> GP.pixbufNewFromFile "cairo-clock-icon.png"
            , #title          := "Haskell-gi Cairo Clock"
            , #defaultWidth   := fromIntegral initialSize
            , #defaultHeight  := fromIntegral initialSize
            ]

  geometry <- new Gdk.Geometry [ #minWidth  := 32
                               , #minHeight := 32
                               , #maxWidth  := 512
                               , #maxHeight := 512 ]
  #setGeometryHints window (Just window) (Just geometry)
                        [Gdk.WindowHintsMinSize, Gdk.WindowHintsMaxSize]

  screen <- window `get` #screen
  visual <- #getRgbaVisual screen
  #setVisual window visual

  on window #keyPressEvent $ \event -> do
    name <- event `get` #keyval >>= Gdk.keyvalName
    when (name == Just "Escape") Gtk.mainQuit
    return False

  on window #buttonPressEvent $ \event -> do
    button <- event `get` #button
    time <- event `get` #time
    x <- event `get` #xRoot
    y <- event `get` #yRoot
    case button of
      1 -> do
        #beginMoveDrag window 1 (round x) (round y) time
        return True
      3 -> do
        #beginResizeDrag window Gdk.WindowEdgeSouthEast 3
                         (round x) (round y) time
        return True
      _ -> return False

  GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000
          (#queueDraw window >> return True)

  backgroundRef <- newIORef (Just undefined)
  foregroundRef <- newIORef (Just undefined)

  let redrawStaticLayers = do
        (width32, height32) <- #getSize window
        let (width, height) = (fromIntegral width32, fromIntegral height32)
        background <- imageSurfaceCreate FormatArgb32 width height
        foreground <- imageSurfaceCreate FormatArgb32 width height
        let clear = do
              save
              setOperator OperatorClear
              paint
              restore
        renderWith background $ do
          clear
          drawClockBackground True width height
        renderWith foreground $ do
          clear
          drawClockForeground True width height
        writeIORef backgroundRef (Just background)
        writeIORef foregroundRef (Just foreground)

  on window #realize redrawStaticLayers

  sizeRef <- newIORef (initialSize, initialSize)
  timeoutHandlerRef <- newIORef Nothing
  on window #configureEvent $ \event -> do
    w <- fromIntegral <$> event `get` #width
    h <- fromIntegral <$> event `get` #height

    size <- readIORef sizeRef
    writeIORef sizeRef (w,h)
    when (size /= (w,h)) $ do

      background <- readIORef backgroundRef
      foreground <- readIORef foregroundRef
      maybe (return ()) surfaceFinish background
      maybe (return ()) surfaceFinish foreground

      writeIORef backgroundRef Nothing
      writeIORef foregroundRef Nothing

      timeoutHandler <- readIORef timeoutHandlerRef
      _ <- maybe (return True) GLib.sourceRemove timeoutHandler

      handler <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT_IDLE 300
                 (do
                   writeIORef timeoutHandlerRef Nothing
                   redrawStaticLayers
                   #queueDraw window
                   return False
                 )
      writeIORef timeoutHandlerRef (Just handler)

    return False

  on window #draw $ \context' -> do
    context <- fromGI context'
    width <- fromIntegral <$> #getAllocatedWidth window
    height <- fromIntegral <$> #getAllocatedHeight window

    background <- readIORef backgroundRef
    foreground <- readIORef foregroundRef

    runRender context $ do
      save
      setOperator OperatorSource
      setSourceRgba 0 0 0 0
      paint
      restore

      case background of
        Nothing -> drawClockBackground False width height
        Just background -> do
          setSourceSurface background 0 0
          paint

      drawClockHands (isJust background) width height

      case foreground of
        Nothing -> drawClockForeground False width height
        Just foreground -> do
          setSourceSurface foreground 0 0
          paint

    return True

  #showAll window

  Gtk.main
