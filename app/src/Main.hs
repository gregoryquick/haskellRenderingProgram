module Main where

import Graphics.UI.Gtk
import Control.Monad.Reader
import Data.IORef

import Rendering
import Structure
import Rasterizer

main = do
    initGUI
    Just screen <- screenGetDefault
    w <- screenGetWidth screen
    h <- screenGetHeight screen
    window <- windowNew
    set window [ containerBorderWidth := 0]
    widgetSetSizeRequest window 20 10
    drawingArea <- drawingAreaNew

    timeoutAddFull (do
      let world = startingWorld
      keepUpdating <- render drawingArea world
      return keepUpdating
      ) priorityDefaultIdle 10

    onExpose drawingArea $ const (do
      let world = startingWorld
      render drawingArea world
      )

    set window [ containerChild := drawingArea]
    widgetShowAll window
    onDestroy window mainQuit
    mainGUI

startingWorld = World (0.5) (0.0)

render :: DrawingArea -> World -> (IO Bool)
render areaToDraw world = do
  window <- widgetGetDrawWindow areaToDraw
  (w,h) <- widgetGetSize areaToDraw
  let param = Paramaters w h
  renderWithDrawable window $ runReader (sketch world inCircle) param
  return True
