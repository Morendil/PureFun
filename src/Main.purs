module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

-- Drawing stuff to the screen
import Graphics.Canvas (Arc, CANVAS, Context2D, arc, fillPath, fillRect, getCanvasElementById,
						getCanvasHeight, getCanvasWidth, getContext2D, setFillStyle)

-- Things that we respond to, updating our state
import Signal (runSignal)
import SignalExtra (foldpE)
import Signal.DOM (animationFrame)
import Signal.Time (Time)

-- Things that we'd rather not deal with
import DOM (DOM)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Timer (TIMER)

type Cell =
  { x :: Number
  , y :: Number
  }

type World = Array Cell

update :: forall e. Time -> World -> Eff (random :: RANDOM | e) World
update time world = traverse (updateCell time) world

updateCell :: forall e. Time -> Cell -> Eff (random :: RANDOM | e) Cell
updateCell time {x: x, y: y} = do
  dx <- map (\q -> 2.0 - q * 4.0) random
  dy <- map (\q -> 2.0 - q * 4.0) random
  pure {x: x+dx , y: y+dy}

initialState :: World
initialState = [{x: 0.0, y: 0.0}, {x: 0.0, y: 0.0}]

main :: forall e. Eff (random :: RANDOM, dom :: DOM, canvas :: CANVAS, timer :: TIMER | e) Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  case mcanvas of
    Just canvas -> do
      width <- getCanvasWidth canvas
      height <- getCanvasHeight canvas
      ctx <- getContext2D canvas
      frames <- animationFrame
      let animate = foldpE update initialState frames
      runSignal (view ctx width height <$> animate)
    Nothing -> pure unit

circle :: Cell -> Arc
circle state =
        { x:  state.x + (640.0 / 2.0)
        , y:  state.y + (480.0 / 2.0)
        , r: 10.0
        , start: 0.0
        , end: 5.0
        }

view :: forall e. Context2D -> Number -> Number -> World -> Eff (canvas :: CANVAS | e) Unit
view ctx width height world = do
  _ <- setFillStyle "#FFFFFF" ctx
  _ <- fillRect ctx { x: 0.0, y: 0.0, w: width, h: height }
  _ <- traverse (viewCell ctx width height) world
  pure unit

viewCell :: forall e. Context2D -> Number -> Number -> Cell -> Eff (canvas :: CANVAS | e) Unit
viewCell ctx width height cell = do
  _ <- setFillStyle "#0000FF" ctx
  _ <- fillPath ctx $ arc ctx $ circle cell
  pure unit
