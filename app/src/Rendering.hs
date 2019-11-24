module Rendering where

import Graphics.Rendering.Cairo
import Control.Monad.Reader

import Structure

data RasterizationParameters = RasterizationParameters
  { horizontalPixels :: Int
  , verticalPixels :: Int
  }

createRasterizationParameters :: Int -> Int -> RasterizationParameters
createRasterizationParameters w h = RasterizationParameters w h

type Generate a = Reader RasterizationParameters a

fillBackround :: Generate (Render ())
fillBackround = do
  (RasterizationParameters w h) <- ask
  return $ do
    setSourceRGBA 0 0 0 1
    rectangle 0 0 (fromIntegral w) (fromIntegral h)
    fill

drawState :: ProgramState -> Generate (Render ())
drawState currentState = do
  (RasterizationParameters w h) <- ask
  return $ do
    let currentTime = stateTime currentState
    let red = 1/(currentTime)
    let blue = 1/(currentTime)
    let green = 1/(currentTime)
    setSourceRGBA red green blue 1
    rectangle ((fromIntegral w)/4) ((fromIntegral h)/4) ((fromIntegral w)/2) ((fromIntegral h)/2)
    fill
