module Neuron.UI where

import Relude

import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Events as E

buttonClass :: String
buttonClass = "text-white focus:ring-4 focus:outline-none font-medium rounded-lg text-sm w-full sm:w-auto px-5 py-2.5 text-center bg-blue-600 hover:bg-blue-700 focus:ring-blue-800"

checkboxClass :: String
checkboxClass = "w-11 h-6 bg-gray-200 rounded-full peer peer-focus:ring-4 peer-focus:ring-blue-300 dark:peer-focus:ring-blue-800 dark:bg-gray-700 peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-0.5 after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all dark:border-gray-600 peer-checked:bg-blue-600"

inputNumberClass :: String
inputNumberClass = "text-white inline-block w-48 border text-4xl rounded-lg p-2.5 bg-gray-700 border-gray-600 placeholder-gray-400 focus:ring-blue-500 focus:border-blue-500"

button ∷ ∀msg. String → msg → Html msg
button name onClick = H.button [ H.class_ buttonClass, E.onClick \_ → onClick ] [H.text name]

dialog ∷ ∀msg. String → Array (Html msg) → Array { name :: String, onClick :: msg } → Html msg
dialog title body buttons =
  H.div [ H.class_ "absolute w-full h-full flex items-center justify-center bg-transp-grey inset-0 z-50" ]
    [ H.div [ H.class_ "bg-black text-white rounded block border-2" ]
        [ H.div [ H.class_ "p-4 min-h-8 border-b-2" ]
            [ H.div [ H.class_ "text-4xl font-medium inline-block" ] [ H.text title ]
            ]
        , H.div [ H.class_ "p-6 border-b-2" ] body
        , H.div [ H.class_ "p-4 text-right" ] $ 
            buttons # mapWithIndex \i {name, onClick} ->
              H.button
                [ H.class_ $ buttonClass <> (if i == 0 then "" else " ml-4")
                , E.onClick \_ -> onClick
                ] [ H.text name ]
        ]
    ]
