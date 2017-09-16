module SignalExtra (foldpE) where

import Signal (Signal, constant)
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn4, runFn4)

foreign import foldpEP  :: forall a b c e. Fn4 (c -> Signal c) (a -> b -> Eff e b) b (Signal a) (Signal b)

foldpE  :: forall a b e. (a -> b -> Eff e b) -> b -> (Signal a) -> (Signal b)
foldpE = runFn4 foldpEP constant
