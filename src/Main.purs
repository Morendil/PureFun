module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, rect, fillPath, setFillStyle, getContext2D, getCanvasElementById)

main :: Eff (canvas :: CANVAS) Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  case mcanvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      _ <- setFillStyle "#0000FF" ctx
      _ <- fillPath ctx $ rect ctx
        { x:  50.0
        , y:  50.0
        , w: 100.0
        , h: 100.0
        }
      pure unit
    Nothing -> pure unit
