module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Pha.App (app)
import Neuron.Model (init)
import Neuron.Update (simulate, update)
import Neuron.View (view)

main :: Effect Unit
main = app { init: { model: simulate init, msg: Nothing }
           , view
           , update
           , selector: "#root"
           }
