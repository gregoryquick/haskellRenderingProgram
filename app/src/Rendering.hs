module Rendering where

import Graphics.UI.Gtk hiding (rectangle, get)
import Graphics.Rendering.Cairo
import Control.Monad.Reader
import Data.Word

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

fillAnimated :: ProgramState -> Generate (Render ())
fillAnimated currentState = do
  (RasterizationParameters w h) <- ask
  return $ do
    let currentTime = stateTime currentState
    let red = 1/(currentTime)
    let blue = 1/(currentTime)
    let green = 1/(currentTime)
    setSourceRGBA red green blue 1
    rectangle ((fromIntegral w)/4) ((fromIntegral h)/4) ((fromIntegral w)/2) ((fromIntegral h)/2)
    fill


drawAllthings :: DrawingArea -> ProgramState -> (IO Bool)
drawAllthings areaToDraw currentState = do
  window <- widgetGetDrawWindow areaToDraw
  (w,h) <- widgetGetSize areaToDraw
  let world = createRasterizationParameters w h
  renderWithDrawable window $ runReader (do
    renderSequence <- sequence [fillBackround,fillAnimated currentState]
    return $ foldr1 (>>) renderSequence
    ) world
  return True
