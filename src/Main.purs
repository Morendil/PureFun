module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomRange)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Array (replicate, (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Graphics.Canvas (CANVAS, Context2D, arc, fillPath, fillRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setFillStyle)
import Signal (runSignal)
import Signal.DOM (animationFrame)
import Signal.Time (Time)
import SignalExtra (foldpE)

data Cell = Cell
  { x :: Number
  , y :: Number
  }
  CellState

data CellState = Stuck | Moving

type World = Array Cell

update :: forall e. Time -> World -> Eff (random :: RANDOM | e) World
update time world = traverse (updateCell time) world

updateCell :: forall e. Time -> Cell -> Eff (random :: RANDOM | e) Cell
updateCell time (Cell {x: x, y: y} Moving) = do
  dx <- randomRange (-1.0) 1.0
  dy <- randomRange (-1.0) 1.0
  pure (Cell {x: x+dx , y: y+dy} Moving)

updateCell time (Cell cell Stuck) = 
  pure (Cell cell Stuck)

makeInitialState :: forall e. Number -> Number -> Eff (random :: RANDOM | e) World
makeInitialState width height =
  let makeCell = do
        x <- randomRange 0.0 width
        y <- randomRange 0.0 height
        pure (Cell {x: x, y: y} Moving)
      makeSeed = pure (Cell {x: width / 2.0, y: height / 2.0} Stuck)
      makeCells = makeSeed : replicate 10 makeCell
  in sequence makeCells

main :: forall e. Eff (random :: RANDOM, dom :: DOM, canvas :: CANVAS, timer :: TIMER | e) Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  case mcanvas of
    Just canvas -> do
      width <- getCanvasWidth canvas
      height <- getCanvasHeight canvas
      ctx <- getContext2D canvas
      frames <- animationFrame
      initialState <- makeInitialState width height
      let animate = foldpE update initialState frames
      runSignal (view ctx width height <$> animate)
    Nothing -> pure unit

view :: forall e. Context2D -> Number -> Number -> World -> Eff (canvas :: CANVAS | e) Unit
view ctx width height world = do
  _ <- setFillStyle "#FFFFFF" ctx
  _ <- fillRect ctx { x: 0.0, y: 0.0, w: width, h: height }
  _ <- traverse (viewCell ctx) world
  pure unit

viewCell :: forall e. Context2D -> Cell -> Eff (canvas :: CANVAS | e) Unit

viewCell ctx (Cell cell Moving) = do
  _ <- setFillStyle "#0000FF" ctx
  _ <- fillPath ctx $ arc ctx $ { x: cell.x, y: cell.y, r: 10.0, start : 0.0, end: 3.14 }
  _ <- setFillStyle "#FF0000" ctx
  _ <- fillPath ctx $ arc ctx $ { x: cell.x, y: cell.y, r: 10.0, start : 3.14, end: 2.0*3.14 }
  pure unit

viewCell ctx (Cell cell Stuck) = do
  _ <- setFillStyle "#00FF00" ctx
  _ <- fillPath ctx $ arc ctx $ { x: cell.x, y: cell.y, r: 10.0, start : 0.0, end: 2.0*3.14 }
  pure unit
