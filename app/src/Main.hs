module Main where

--import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import System.Directory
import Control.Monad.Reader

import Rendering

main = do
    let world = RasterizationParameters 50 50
    --surface <- createImageSurface FormatARGB32 (horizontalPixels world) (verticalPixels world)
    --renderWith surface $ runReader sketch world
    surface <- imageSurfaceCreateFromPNG "/data/in.png"
    renderWith surface fillSquare
    surfaceWriteToPNG surface "/data/out.png"
    getLine
    removeFile "/data/out.png"
--    initGUI
--    window <- windowNew
--    button <- buttonNew
--    set window [ containerBorderWidth := 10, containerChild := button ]
--    set button [ buttonLabel := "Hello World" ]
--    onClicked button (putStrLn "Hello World")
--    onDestroy window mainQuit
--    widgetShowAll window
--    mainGUI

fillSquare :: Render ()
fillSquare = do
  setSourceRGBA 1 1 0 1
  rectangle 20 20 10 10
  fill
