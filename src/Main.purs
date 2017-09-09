module Main where

import Prelude
import Data.Maybe (Maybe(..))

-- Drawing stuff to the screen
import Graphics.Canvas (CANVAS, Context2D, Arc, arc, fillPath, setFillStyle, getContext2D, getCanvasElementById)

-- Things that we respond to, updating our state
import Signal (foldp, runSignal)
import Signal.DOM (animationFrame)
import Signal.Time (Time)

-- Things that we'd rather not deal with
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)

type World =
  { x :: Number
  , y :: Number
  , time :: Time
  }

update :: Time -> World -> World
update time {x, y, time: 0.0} = {x: x, y: y, time: time}
update time {x, y, time: before} = {x: x + (time-before)*0.01, y: y + (time-before)*0.01, time: time}

initialState :: World
initialState = {x: 0.0, y: 0.0, time: 0.0}

main :: forall e. Eff (canvas :: CANVAS, dom :: DOM, timer :: TIMER | e) Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  case mcanvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      frames <- animationFrame
      let animate = foldp update initialState frames
      runSignal (view ctx <$> animate)
    Nothing -> pure unit

circle :: World -> Arc
circle state = 
        { x:  state.x + (640.0 / 2.0)
        , y:  state.x + (480.0 / 2.0)
        , r: 10.0
        , start: 0.0
        , end: 5.0
        }

view :: forall e. Context2D -> World -> Eff (canvas :: CANVAS | e) Unit
view ctx state = do
  _ <- setFillStyle "#0000FF" ctx
  _ <- fillPath ctx $ arc ctx $ circle state
  pure unit
