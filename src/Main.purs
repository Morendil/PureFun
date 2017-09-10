module Main where

import Prelude
import Data.Maybe (Maybe(..))

-- Drawing stuff to the screen
import Graphics.Canvas (Arc, CANVAS, Context2D, arc, fillPath, fillRect, getCanvasElementById,
						getCanvasHeight, getCanvasWidth, getContext2D, setFillStyle)

-- Things that we'd rather not deal with
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef, Ref)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (Window)
import DOM.HTML.Window (requestAnimationFrame)

type World =
  { x :: Number
  , y :: Number
  }

type Move =
  { dx :: Number
  , dy :: Number
  }

randomMove :: forall e. Eff (random :: RANDOM | e) Move
randomMove = do
	dx <- random
	dy <- random
	pure {dx: dx, dy: dy}

initialState :: World
initialState = {x: 0.0, y: 0.0}

main :: Eff _ Unit
main = withAnimateContext "canvas" initialState animate

animate ctx width height state = do
  move <- randomMove
  _ <- view ctx width height state

  -- Update the state
  pure $ state { x = state.x + move.dx - 0.5
               , y = state.y + move.dy - 0.5 }

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

loopAnimation :: forall e state.
  Window ->
  Ref state ->
  state ->
  (state -> Eff _ state) ->
  Eff _ Unit
loopAnimation window ref state step =
  void $ requestAnimationFrame
    do loopAnimation window ref state step
       state1 <- readRef ref
       state2 <- step state1
       writeRef ref state2
    window

withAnimation :: forall e state.
  state ->
  (state -> Eff _ state) ->
  Eff _ Unit
withAnimation state step = do
  window <- window
  ref <- newRef state
  loopAnimation window ref state step


withAnimateContext :: forall e state.
  String ->
  state ->
  (Context2D -> Number -> Number -> state -> Eff _ state) ->
  Eff _ Unit
withAnimateContext name state draw = do
  canvas <- getCanvasElementById name
  case canvas of
    Just canvas1 -> do
      width <- getCanvasWidth canvas1
      height <- getCanvasHeight canvas1
      ctx <- getContext2D canvas1
      withAnimation state $ draw ctx width height
    Nothing -> pure unit
