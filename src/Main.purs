module Main where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomRange)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Data.Array (any, filter, foldl, replicate, (:))
import Data.Int (round)
import Data.Map (Map, empty, alter, lookup)
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Traversable (sequence, traverse)
import Graphics.Canvas (CANVAS, Context2D, arc, fillPath, fillRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setFillStyle)
import Math (sqrt)
import Signal (runSignal)
import Signal.DOM (animationFrame)
import Signal.Time (Time)
import SignalExtra (foldpE)

type Position = { x :: Number, y :: Number}
data Cell = Cell Position CellState
data CellState = Stuck | Moving

type World = Array Cell
type Collider = Map (Pair Int) (Array Cell)

update :: forall e. Time -> World -> Eff (random :: RANDOM | e) World
update time world = map freezeCells $ traverse (updateCell time) world

toCollider :: Array Cell -> Collider
toCollider world =
  let bucket x = (round x) / 20
      cellKeys (Cell pos _) = lift2 Pair [(bucket pos.x)-1,(bucket pos.x),(bucket pos.x)+1] [(bucket pos.y)-1,(bucket pos.y),(bucket pos.y)+1]
      addCell x Nothing = Just [x]
      addCell x (Just xs) = Just (x : xs)
  in foldl (\dict cell -> foldl (\dict1 key -> alter (addCell cell) key dict1) dict $ cellKeys cell) empty world

freezeCells :: World -> World
freezeCells world =
  let stuckCells = filter stuck world
      collider = toCollider stuckCells
      bucket x = (round x) / 20
      cellKey (Cell pos _) = Pair (bucket pos.x) (bucket pos.y)
      freezeStray cell = case lookup (cellKey cell) collider of
        Nothing -> cell
        Just cells -> if collide cells cell then freeze cell else cell
  in map freezeStray world

freeze :: Cell -> Cell
freeze (Cell state _) = Cell state Stuck

stuck :: Cell -> Boolean
stuck (Cell _ Stuck) = true
stuck (Cell _ Moving) = false

collide :: Array Cell -> Cell -> Boolean
collide cells cell =
  let collideWith (Cell pos1 _) (Cell pos2 _) = distance pos1 pos2 < 20.0
  in any (collideWith cell) cells

distance :: Position -> Position -> Number
distance pos cell = sqrt $ (cell.x-pos.x)*(cell.x-pos.x) + (cell.y-pos.y)*(cell.y-pos.y)

updateCell :: forall e. Time -> Cell -> Eff (random :: RANDOM | e) Cell

updateCell time (Cell {x: x, y: y} Moving) = do
  dx <- randomRange (-2.0) 2.0
  dy <- randomRange (-2.0) 2.0
  pure (Cell {x: x+dx , y: y+dy} Moving)

updateCell time (Cell cell Stuck) = 
  pure (Cell cell Stuck)

makeInitialState :: forall e. Number -> Number -> Eff (random :: RANDOM | e) World
makeInitialState width height =
  let makeCell = do
        x <- randomRange 0.0 width
        y <- randomRange 0.0 height
        pure (Cell {x: x-width/2.0, y: y-height/2.0} Moving)
      makeSeed = pure (Cell {x: 0.0, y: 0.0} Stuck)
      makeCells = makeSeed : replicate 500 makeCell
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
  _ <- traverse (viewCell ctx width height) world
  pure unit

viewCell :: forall e. Context2D -> Number -> Number -> Cell -> Eff (canvas :: CANVAS | e) Unit

viewCell ctx width height (Cell cell Moving) = do
  _ <- setFillStyle "#0000FF" ctx
  _ <- fillPath ctx $ arc ctx $ { x: cell.x + width / 2.0, y: cell.y + height / 2.0, r: 10.0, start : 0.0, end: 3.14 }
  _ <- setFillStyle "#FF0000" ctx
  _ <- fillPath ctx $ arc ctx $ { x: cell.x + width / 2.0, y: cell.y + height / 2.0, r: 10.0, start : 3.14, end: 2.0*3.14 }
  pure unit

viewCell ctx width height (Cell cell Stuck) = do
  _ <- setFillStyle "#00FF00" ctx
  _ <- fillPath ctx $ arc ctx $ { x: cell.x + width / 2.0, y: cell.y + height / 2.0, r: 10.0, start : 0.0, end: 2.0*3.14 }
  pure unit
