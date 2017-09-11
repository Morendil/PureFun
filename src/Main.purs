module Main where

import Prelude
import Control.Apply ((*>))
import Signal (Signal(), unwrap)
import Signal.Time (every, second)
import Flare (UI(), button, foldp, runFlareShow, lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random
import Control.Apply (lift2)

tick :: Signal (Eff _ Int)
tick = every second *> pure (randomInt 1 10)

-- foldp :: forall a b e. (a -> b -> b) -> b -> UI e a -> UI e b

score :: UI _ Int
score = foldp (+) 0 $ lift2 (+) (lift $ unwrap tick) (button "Click me!" 0 1)

-- runFlareShow :: forall e a. Show a => ElementId -> ElementId -> UI e a -> Eff (dom :: DOM, channel :: CHANNEL | e) Unit

main = runFlareShow "controls" "output" score
