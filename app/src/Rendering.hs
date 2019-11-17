module Rendering where

import Graphics.Rendering.Cairo
import Control.Monad.Reader
import Data.Word

data RasterizationParameters = RasterizationParameters
  { horizontalPixels :: Int
  , verticalPixels :: Int
  -- , horizontalPixelSpacing :: Double
  -- , verticalPixelSpacing :: Double
  -- , renderOriginX :: Double
  -- , renderOriginY :: Double
  }

data RGBAValue = RGBAValue
  { red :: Word8
  , green :: Word8
  , blue :: Word8
  , alpha :: Word8
  }

type ColourFunction = Double -> Double -> RGBAValue

type Generate a = Reader RasterizationParameters a

fillDefualtColour :: Generate (Render ())
fillDefualtColour = do
  (RasterizationParameters w h) <- ask
  return $ do
    setSourceRGBA 0 0 0 1
    rectangle 0 0 (fromIntegral w) (fromIntegral h)
    fill

drawSquare :: Generate (Render ())
drawSquare = do
  return $ do
    setSourceRGBA 1 1 0 1
    rectangle 10 10 10 10
    fill

black = RGBAValue 0 0 0 1

canvas :: Generate (Render ())
canvas = do
  renderSequence <- sequence [fillDefualtColour]
  return $ foldr1 (>>) renderSequence

--sketch :: Generate (ColourFunction) -> Generate (Render ())
--sketch = do
--  (RasterizationParameters w h) <- ask
--  return $ do
--    createImageSurface FormatRGB24 w h
