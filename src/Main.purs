module Main where

import Prelude
import Effect (Effect)
import Pha.App (sandbox)
import Neuron.Model (init, update)
import Neuron.View (view)

main :: Effect Unit
main = sandbox { init, view, update, selector: "#root" }
