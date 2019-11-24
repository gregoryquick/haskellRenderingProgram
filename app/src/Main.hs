module Main where

import Graphics.UI.Gtk
import Control.Monad.Reader
import Data.IORef

import Rendering
import Structure

main = do
    initGUI
    Just screen <- screenGetDefault
    w <- screenGetWidth screen
    h <- screenGetHeight screen
    window <- windowNew
    set window [ containerBorderWidth := 0]
    widgetSetSizeRequest window 20 10
    drawingArea <- drawingAreaNew
    globalState <- newIORef $ ProgramState 1.0

    timeoutAddFull (do
      currentState <- readIORef globalState
      keepUpdating <- render drawingArea currentState
      modifyIORef' globalState tickTimeForward
      return keepUpdating
      ) priorityDefaultIdle 10

    onExpose drawingArea $ const (do
      currentState <- readIORef globalState
      render drawingArea currentState
      )

    set window [ containerChild := drawingArea]
    widgetShowAll window
    onDestroy window mainQuit
    mainGUI

render :: DrawingArea -> ProgramState -> (IO Bool)
render areaToDraw currentState = do
  window <- widgetGetDrawWindow areaToDraw
  (w,h) <- widgetGetSize areaToDraw
  let world = createRasterizationParameters w h
  renderWithDrawable window $ runReader (do
    renderSequence <- sequence [fillBackround,drawState currentState]
    return $ foldr1 (>>) renderSequence
    ) world
  return True
