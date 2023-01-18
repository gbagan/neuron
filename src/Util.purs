module Neuron.Util where

import Prelude

import Data.Array (foldl, zipWith, unsafeIndex)
import Partial.Unsafe (unsafePartial)

count :: ∀a. (a → Boolean) → Array a → Int
count f = foldl (\acc x -> if f x then acc + 1 else acc) 0

map2 ∷ ∀a b c. Array a → Array b → (a → b → c) → Array c
map2 t1 t2 fn = zipWith fn t1 t2

indexImpl :: ∀a. Array a → Int → a
indexImpl a i = unsafePartial (unsafeIndex a i)

infixl 8 indexImpl as !