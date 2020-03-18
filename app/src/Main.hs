module Main where

import Graphics.UI.Gtk hiding (HasTime)
import Control.Monad.Reader
import Graphics.Rendering.Cairo
import Control.Wire
import Data.Time
import FRP.Netwire
import Prelude hiding ((.), id, until)
import Control.Wire.Unsafe.Event
import Control.Monad.IO.Class
import Data.Fixed
import Data.List

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

render :: DrawingArea -> World -> (IO Bool)
render areaToDraw world = do
  window <- widgetGetDrawWindow areaToDraw
  (w,h) <- widgetGetSize areaToDraw
  let param = Paramaters w h
  renderWithDrawable window $ runReader (sketch world inCircle) param
  return True

--Netwire code

startingWorld = World (0.5) (0.0)

convertList :: [(Double,World)] -> [(Double, IO ())]
convertList myList = fmap (\x -> (fst x, print (snd x))) myList

worldStream :: (HasTime t s, Monad m, Fractional t) => Wire s () m a (World)
worldStream = asSoonAs . eventStream

eventStream :: (HasTime t s, Monad m, Fractional t) => Wire s e m a (Event (World))
eventStream = createEvents $ fmap (\x -> (convert $ fst x, snd x)) $ worlds startingWorld
  where
    convert = fromRational . toRational

createEvents :: (HasTime t s) => [(t,b)] -> Wire s e m a (Event b)
createEvents [] = never
createEvents (x:xs) = mkSFN $ \_ -> (Event (snd x), loop (fst x) xs)
   where
    loop _ [] = never
    loop 0 xs = loop (fst x) xs
    loop t' xs0@(x:xs) =
        mkSF $ \ds _ ->
            let t = t' - dtime ds
            in if t <= 0
                 then (Event (snd x), loop (mod' t (fst x)) xs)
                 else (NoEvent, loop t xs0)

run :: (HasTime t s, MonadIO m) => Session m s -> Wire s e m () (Event (IO ())) -> m e
run = go
  where
    go session wire = do
      (dt, session') <- stepSession session
      (wt', wire') <- stepWire wire dt (Right ())
      case wt' of
        Left a -> return a
        Right bEvent -> do
          case bEvent of
            Event b -> liftIO $ b
            _ -> return ()
          go session' wire'
