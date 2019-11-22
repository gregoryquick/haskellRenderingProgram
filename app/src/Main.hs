{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Graphics.UI.Gtk hiding (rectangle, get)
import Graphics.Rendering.Cairo
import System.Directory
import Control.Monad.Reader
import Control.Monad.State
import Data.Array.IArray
import Data.Array.MArray
import Data.Word

import Rendering

main = do
    initGUI
    window <- windowNew
    set window [ containerBorderWidth := 10]
    drawingArea <- drawingAreaNew
    onExpose drawingArea $ const (updateImage drawingArea)
    set window [ containerChild := drawingArea]
    widgetShowAll window
    onDestroy window mainQuit
    mainGUI

updateImage :: DrawingArea -> IO Bool
updateImage areaToDraw = do
  window <- widgetGetDrawWindow areaToDraw
  (w,h) <- widgetGetSize areaToDraw
  let world = RasterizationParameters w h
  renderWithDrawable window $ runReader (evalState fillAnimated 1) world
  return True

fillAnimated :: State Double (Generate (Render ()))
fillAnimated = do
  time <- get
  put $ time + 0.01
  return $ do
    (RasterizationParameters w h) <- ask
    let red = 1/time
    let blue = 1/time
    let green = 1/time
    return $ do
      setSourceRGBA red green blue 1
      rectangle ((fromIntegral w)/4) ((fromIntegral h)/4) ((fromIntegral w)/2) ((fromIntegral w)/2)
      fill

fillSquare :: Generate (Render ())
fillSquare = do
  (RasterizationParameters w h) <- ask
  return $ do
    setSourceRGBA 1 1 0 1
    rectangle ((fromIntegral w)/4) ((fromIntegral h)/4) ((fromIntegral w)/2) ((fromIntegral w)/2)
    fill

--animation :: State Double [Generate (Render ())]
--animation = map (const $ fillAnimated) [0 ..]

getPixel :: Surface -> Int -> Int -> IO Word32
getPixel surface x y = do
  stride <- imageSurfaceGetStride surface
  let i = y * (div stride 4) + x
  (pixelData :: SurfaceData Int Word32) <- imageSurfaceGetPixels surface
  readArray pixelData i >>= return
