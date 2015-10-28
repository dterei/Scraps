-- http://learn.hfm.io/fractals.html
module Fractals where

import Control.Monad
import Codec.Picture hiding (imageHeight, imageWidth)
import Graphics.Rasterific hiding (Point, Vector, Line, Path, polygon)
import Graphics.Rasterific.Texture

-- some constants
imageWidth, imageHeight :: Int
imageWidth = 800
imageHeight = 800

background :: Colour
background = black

-- our drawing types
type Point  = (Float, Float)
type Vector = (Float, Float)
type Line = (Point, Point)
type Path = [Point]
type Colour = (Int, Int, Int, Int)
type Picture = [(Colour, Path)]

-- some predefined colours
white, black, blue, red, green, orange, magenta, lightgreen, darkblue :: Colour
white      = (200,  200, 255, 255)
black      = (  0,    0,   0, 255)
blue       = (  0,  110, 255, 255)
red        = (255,    0,   0, 255)
green      = (10,  255,  10,  235)
orange     = (255,  255,  0,  200)
magenta    = (153,    0, 153, 220)
lightgreen = ( 27,  230,  34, 255)
darkblue   = ( 24,   50, 194, 255)

-- render a picture
drawPicture :: Float -> Picture -> Image PixelRGBA8
drawPicture linewidth picture
  = renderDrawing imageWidth imageHeight (toColour background) $
      forM_ picture $
        \(col, p) -> withTexture (uniformTexture $ toColour col) (drawPath p)
  where
    drawPath points    = stroke linewidth JoinRound (CapRound, CapStraight 0) $
                           polyline (map (uncurry V2) points)
    toColour (a,b,c,d) = PixelRGBA8 (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)

-- save a picture
savePicture :: FilePath -> Float -> Picture -> IO ()
savePicture path lineWidth picture
  = savePngImage path $ ImageRGBA8 img
  where img = drawPicture lineWidth picture

-- some drawings
house :: Path
house = [(300, 750), (300, 450), (270, 450), (500, 200),
         (730, 450), (700, 450), (700, 750)]

rotateLine :: Float -> Line -> Line
rotateLine alpha ((x1, y1), (x2, y2))
  = ((x1, y1), (x' + x1, y' + y1))
  where
    x0 = x2 - x1
    y0 = y2 - y1
    x' = x0 * cos alpha - y0 * sin alpha
    y' = x0 * sin alpha + y0 * cos alpha

scaleLine :: Float -> Line -> Line
scaleLine factor ((x1, y1), (x2, y2))
  = ((x1, y1), (x' + x1, y' + y1))
  where
    x0 = x2 - x1
    y0 = y2 - y1
    x' = factor * x0
    y' = factor * y0

fade :: Colour -> Colour
fade (r,g,b,a) = (r,g,b,a-1)

spiralRays :: Float -> Float -> Int -> Colour -> Line -> Picture
spiralRays angle scale = spiralRays'
  where
    spiralRays' n colour l@(p1, p2)
      | n <= 0    = []
      | otherwise = (colour, [p1, p2]) : spiralRays' (n-1) newColour newLine
      where
        newColour = fade colour
        newLine   = scaleLine scale $ rotateLine angle l
