module Main where

import Graphics.UI.Gtk
--import Graphics.Rendering.Cairo

import Rendering

--main = do
--  surface <- createImageSurface FormatARGB32 500 500
--  renderWith surface sketch
--  surfaceWriteToPNG surface "out.png"
main = do
    initGUI
    window <- windowNew
    button <- buttonNew
    set window [ containerBorderWidth := 10, containerChild := button ]
    set button [ buttonLabel := "Hello World" ]
    onClicked button (putStrLn "Hello World")
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
