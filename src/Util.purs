module Neuron.Util
  where

import Prelude
import Data.Array (zipWith, mapWithIndex)

map2 ∷ ∀a b c. Array a → Array b → (Int → a → b → c) → Array c
map2 t1 t2 fn = zipWith ($) (mapWithIndex fn t1) t2
