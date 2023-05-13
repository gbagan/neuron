module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Pha.App (app)
import Neuron.Model (init)
import Neuron.Update (simulate, update)
import Neuron.View (view)

main :: Effect Unit
main = app { init: { state: simulate init, action: Nothing }
           , eval: identity
           , view
           , update
           , subscriptions: []
           , selector: "#root"
           }
