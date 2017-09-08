module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

greet :: String -> String
greet name = "Hello, " <> name <> "!"

main = unsafePerformEff $ log (greet "World")
