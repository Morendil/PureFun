module Main where

import Prelude
import Data.Maybe (Maybe(..))

-- Drawing stuff to the screen
import Graphics.Canvas (Arc, CANVAS, Context2D, arc, fillPath, fillRect, getCanvasElementById,
						getCanvasHeight, getCanvasWidth, getContext2D, setFillStyle)

-- Things that we respond to, updating our state
import Signal (foldp, runSignal)
import Signal.DOM (animationFrame)
import Signal.Time (Time)
import Math (sin, cos)

-- Things that we'd rather not deal with
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)

type World =
  { x :: Number
  , y :: Number
  , start :: Time
  }

update :: Time -> World -> World
update time {x, y, start: 0.0} = {x: x, y: y, start: time}
update time {x, y, start: start} = {x: sin(time/100.0)*(time-start)/1000.0, y: cos(time/100.0)*(time-start)/1000.0, start: start}

initialState :: World
initialState = {x: 0.0, y: 0.0, start: 0.0}

main :: forall e. Eff (canvas :: CANVAS, dom :: DOM, timer :: TIMER | e) Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  case mcanvas of
    Just canvas -> do
      width <- getCanvasWidth canvas
      height <- getCanvasHeight canvas
      ctx <- getContext2D canvas
      frames <- animationFrame
      -- foldp :: forall a b. (a -> b -> b) -> b -> (Signal a) -> (Signal b)
      -- animate :: Signal World
      let animate = foldp update initialState frames
      -- map :: forall a b. (a -> b) -> f a -> f b
      -- (World -> Eff _ Unit) -> Signal World -> Signal (Eff _ Unit)
      -- runSignal :: forall e. Signal (Eff e Unit) -> Eff e Unit
      runSignal (view ctx width height <$> animate)
    Nothing -> pure unit

circle :: World -> Arc
circle state =
        { x:  state.x + (640.0 / 2.0)
        , y:  state.y + (480.0 / 2.0)
        , r: 10.0
        , start: 0.0
        , end: 5.0
        }

view :: forall e. Context2D -> Number -> Number -> World -> Eff (canvas :: CANVAS | e) Unit
view ctx width height state = do
  _ <- setFillStyle "#FFFFFF" ctx
  _ <- fillRect ctx { x: 0.0, y: 0.0, w: width, h: height }
  _ <- setFillStyle "#0000FF" ctx
  _ <- fillPath ctx $ arc ctx $ circle state
  pure unit
