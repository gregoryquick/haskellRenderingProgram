module Rendering(sketch) where

import Graphics.Rendering.Cairo

bg :: Render ()
bg = do
  setSourceRGBA 0 0 0 1
  rectangle 0 0 500 500
  fill

drawSquare :: Render ()
drawSquare = do
  setSourceRGBA 1 1 0 1
  rectangle 10 10 100 100
  fill

sketch :: Render ()
sketch = bg >> drawSquare
