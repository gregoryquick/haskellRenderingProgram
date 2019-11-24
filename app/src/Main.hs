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
import Data.IORef

import Rendering

main = do
    initGUI
    Just screen <- screenGetDefault
    w <- screenGetWidth screen
    h <- screenGetHeight screen
    window <- windowNew
    set window [ containerBorderWidth := 0]
    widgetSetSizeRequest window 20 10
    drawingArea <- drawingAreaNew
    globalState <- newIORef $ 1.0
    timeoutAddFull (do
      time <- readIORef globalState
      let newState = runState (updateImage drawingArea) time
      modifyIORef' globalState (\_ -> snd newState)
      runStill <- fst newState
      return runStill) priorityDefaultIdle 10

    onExpose drawingArea $ const (do
      time <- readIORef globalState
      runStill <- evalState (updateImage drawingArea) time
      return runStill
      )
    -- globalState <- newIORef $ runState (updateImage drawingArea) 1
    -- onExpose drawingArea $ const (modifyIORef' globalState (updateState drawingArea) >> fst (readIORef globalState))
    set window [ containerChild := drawingArea]
    widgetShowAll window
    onDestroy window mainQuit
    mainGUI

-- updateImage :: DrawingArea -> State Double (IO Bool)
-- updateImage areaToDraw = do
--   time <- get
--   put $ time + 0.01
--   return $ drawAllthings areaToDraw time

updateImage :: DrawingArea -> State Double (IO Bool)
updateImage areaToDraw = state $ \currentTime -> (drawAllthings areaToDraw currentTime, currentTime + 0.001)

drawAllthings :: DrawingArea -> Double -> IO Bool
drawAllthings areaToDraw currentTime = do
  window <- widgetGetDrawWindow areaToDraw
  (w,h) <- widgetGetSize areaToDraw
  let world = createRasterizationParameters w h
  renderWithDrawable window $ runReader (do
    renderSequence <- sequence [fillBackround,fillAnimated currentTime]
    return $ foldr1 (>>) renderSequence
    ) world
  return True

fillAnimated :: Double -> Generate (Render ())
fillAnimated currentTime = do
  (RasterizationParameters w h) <- ask
  return $ do
    let red = 1/currentTime
    let blue = 1/currentTime
    let green = 1/currentTime
    setSourceRGBA red green blue 1
    rectangle ((fromIntegral w)/4) ((fromIntegral h)/4) ((fromIntegral w)/2) ((fromIntegral h)/2)
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
