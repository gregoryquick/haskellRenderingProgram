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
      modifyIORef' globalState tickTimeForward
      drawAllthings drawingArea currentState
      ) priorityDefaultIdle 10

    onExpose drawingArea $ const (do
      currentState <- readIORef globalState
      drawAllthings drawingArea currentState
      )

    set window [ containerChild := drawingArea]
    widgetShowAll window
    onDestroy window mainQuit
    mainGUI
