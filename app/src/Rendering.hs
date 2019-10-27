module Rendering where

import Graphics.Rendering.Cairo
import Control.Monad.Reader

data RasterizationParameters = RasterizationParameters
  { horizontalPixels :: Int
  , verticalPixels :: Int
  -- , horizontalPixelSpacing :: Double
  -- , verticalPixelSpacing :: Double
  -- , renderOriginX :: Double
  -- , renderOriginY :: Double
  }

data RGBAValue = RGBAValue
  { red :: Double
  , blue :: Double
  , green :: Double
  , alpha :: Double
  }

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

sketch :: Generate (Render ())
sketch = do
  renderSequence <- sequence [fillDefualtColour, drawSquare]
  return $ foldr1 (>>) renderSequence
