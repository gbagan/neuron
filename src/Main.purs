module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Pha.App (app)
import Neuron.Model (init)
import Neuron.Update (update)
import Neuron.View (view)

main :: Effect Unit
main = app { init: { state: init, action: Nothing}
           , eval: identity
           , view
           , update
           , subscriptions: []
           , selector: "#root"
           }
