module Neuron.Util where

import Prelude

import Data.Array (filter, length, zipWith, mapWithIndex, unsafeIndex)
import Partial.Unsafe (unsafePartial)

count :: ∀a. (a → Boolean) → Array a → Int
count f = length <<< filter f 

map2 ∷ ∀a b c. Array a → Array b → (Int → a → b → c) → Array c
map2 t1 t2 fn = zipWith ($) (mapWithIndex fn t1) t2

index' :: ∀a. Array a → Int → a
index' a i = unsafePartial (unsafeIndex a i)

infixl 8 index' as !!!