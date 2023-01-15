module Neuron.View (view) where

import Prelude

import Data.Array ((..), (!!), concat, mapWithIndex, intersperse)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Neuron.Model (Dialog(..), Model, Msg(..), Neuron(..), Pattern)
import Neuron.Util (map2, (!!!))
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc, translate)

buttonClass :: String
buttonClass = "text-white focus:ring-4 focus:outline-none font-medium rounded-lg text-sm w-full sm:w-auto px-5 py-2.5 text-center bg-blue-600 hover:bg-blue-700 focus:ring-blue-800"

inputRange :: String
inputRange = "inline-block w-48 border text-4xl rounded-lg p-2.5 bg-gray-700 border-gray-600 placeholder-gray-400 text-white focus:ring-blue-500 focus:border-blue-500"

showEditor :: Model -> Html Msg
showEditor {patterns, currentPattern} =
    H.div [H.class_ "absolute w-full h-full flex items-center justify-center bg-transp-grey inset-0 z-50"]
    [   H.div [H.class_ "bg-black text-white rounded block border-2"]
        [   H.div [H.class_ "p-4 min-h-8 border-b-2"]
            [   H.div [H.class_ "text-2xl font-medium inline-block"] [H.text "Modifier le motif"]
            ]
        ,   H.div [H.class_ "p-6 border-b-2"] [body]
        ,   H.div [H.class_ "p-4 text-right"] --[H.class_ "ui-dialog-buttons"]
            [   H.button 
                [   H.class_ $ buttonClass <> " mr-4"
                ,   E.onClick \_ -> ResetPattern
                ]
                [   H.text "Réinitialiser"
                ]
            ,   H.button
                [   H.class_ buttonClass
                ,   E.onClick \_ -> OpenDialog NoDialog
                ]
                [   H.text "Ok"
                ]
            ]
        ]
    ]
    where
    body =
        H.div [H.class_ "flex flex-row items-center gap-8"]
        [   editor
        ,   H.div [H.class_ "grid grid-cols-6 gap-2"] $
                patterns # mapWithIndex \i pattern ->
                    drawMiniPattern i (i == currentPattern) pattern
        ]

    editor = H.maybe (patterns !! currentPattern) \{pattern} ->
        H.div [H.class_ "m-2 overflow-hidden relative border-4 w-64 h-96"] $
            pattern # mapWithIndex \i b ->
                let row = i `div` 6
                    col = i `mod` 6
                in
                H.div
                [   H.class_ "absolute pattern-pixel"
                ,   H.class' "black" b
                ,   H.style "width" "16.66666666%"
                ,   H.style "height" "11.1111111111%"
                ,   H.style "left" $ pc $ Int.toNumber col / 6.0
                ,   H.style "top" $ pc $ Int.toNumber row / 9.0
                ,   E.onClick \_ -> ChangePixel i
                ] []

neuronDialog :: Model -> Int -> Html Msg
neuronDialog {editMode, states, currentState} i =
    H.div [H.class_ "absolute w-full h-full flex items-center justify-center bg-transp-grey inset-0 z-50"]
    [   H.div [H.class_ "bg-black text-white rounded block border-2"]
        [   H.div [H.class_ "p-4 min-h-8 border-b-2"]
            [   H.div [H.class_ "text-2xl font-medium inline-block"] [H.text "Modifier le motif"]
            ]
        ,   H.div [H.class_ "p-6 border-b-2"] [body]
        ,   H.div [H.class_ "p-4 text-right"]
            [   H.button 
                [   H.class_ $ buttonClass <> " mr-4"
                ,   E.onClick \_ -> ToggleEditMode
                ]
                [   H.text "Editer"
                ]
            ,   H.button
                [   H.class_ buttonClass
                ,   E.onClick \_ -> OpenDialog NoDialog
                ]
                [   H.text "Ok"
                ]
            ]
        ]
    ]
    where
    {neurons} = states !!! currentState
    body = H.div [H.class_ "text-4xl"] $
        case neurons !!! i of
            Input _ -> []
            Neuron {coeffs, threshold} -> 
                [   if editMode then
                        H.span [] $ intersperse (H.span [H.class_ "text-4xl mx-4"] [H.text "+"]) $
                            coeffs # mapWithIndex \j {from, coeff} ->
                                H.span []
                                [   H.input
                                    [   H.class_ inputRange
                                    ,   P.type_ "number"
                                    ,   P.value (show coeff)
                                    ,   E.onValueChange $ ChangeCoeff i j
                                    ]
                                ,   showNeuron from
                                ]
                    else
                        H.span [] $ concat $ coeffs # mapWithIndex \j {from, coeff} ->
                            [   H.text $ (if j > 0 && coeff >= 0.0 then "+" else "") <> show coeff
                            ,   showNeuron from
                            ]
                ,   H.br
                ,   H.br
                ,   H.text "Seuil: "
                ,   if editMode then
                        H.input
                        [   H.class_ inputRange
                        ,   P.type_ "number"
                        ,   P.value (show threshold)
                        ,   E.onValueChange $ ChangeThreshold i
                        ]
                    else
                        H.text (show threshold)
                ]
    showNeuron j =
        H.div [H.class_ "inline-block w-8 h-8"]
        [   H.svg [H.class_ "w-full h-full"]
            [   H.use [P.href $ "#neuron-" <>  show j]]
        ]

drawPattern :: Maybe Int -> Pattern -> Html Msg
drawPattern selectedCaptor {pattern} =
    H.div [H.class_ "overflow-hidden relative border-4 border-slate-400 m-2 w-64 h-96"] $
        pattern # mapWithIndex \i b ->
            let row = i `div` 6
                col = i `mod` 6
                capt = Just (row `div` 3 + 3) == selectedCaptor || Just (col `div` 2) == selectedCaptor 
            in
            H.div
            [   H.class_ "absolute pattern-pixel"
            ,   H.class' "black" b
            ,   H.class' "selected" capt
            ,   H.style "width" "16.66666666%"
            ,   H.style "height" "11.1111111111%"
            ,   H.style "left" $ pc $ Int.toNumber col / 6.0
            ,   H.style "top" $ pc $ Int.toNumber row / 9.0
            ] []

drawMiniPattern :: Int -> Boolean -> Pattern -> Html Msg
drawMiniPattern index isCurrent {symbol, pattern} =
    H.div
    [   H.class_ "overflow-hidden relative border-4 border-slate-400 m-2 w-24 h-36"
    ,   H.class_ if isCurrent then "border-green-500" else "border-slate-400" 
    ,   E.onClick \_ -> SelectPattern index
    ] $
        pattern # mapWithIndex \i b ->
            let row = i `div` 6
                col = i `mod` 6
                color = case symbol, b of
                    _, false -> "bg-white"
                    0, _ -> "bg-green-400"
                    1, _ -> "bg-blue-400"
                    2, _ -> "bg-pink-500"
                    _, _ -> "bg-yellow-400"
            in
            H.div
            [   H.class_ $ "absolute " <> color
            ,   H.style "width" "16.66666666%"
            ,   H.style "height" "11.1111111111%"
            ,   H.style "left" $ pc $ Int.toNumber col / 6.0
            ,   H.style "top" $ pc $ Int.toNumber row / 9.0
            ] []            

drawLine :: forall a. Int -> Int -> Html a
drawLine i j =
    let layer = i `div` 6
        row = i `mod` 6
        layer' = j `div` 6
        row' = j `mod` 6
    in
    H.line
    [   P.y1 $ if layer <= 1 then 10.0 + 15.0 * Int.toNumber row else 20.0 + 20.0 * Int.toNumber row 
    ,   P.y2 $ if layer' <= 1 then 10.0 + 15.0 * Int.toNumber row' else 20.0 + 20.0 * Int.toNumber row'
    ,   P.x1 $ if layer == 0 then 15.0 else if layer == 1 then 60.0 else 105.0
    ,   P.x2 $ if layer' == 0 then 15.0 else if layer' == 1 then 60.0 else 105.0
    ,   P.strokeWidth 0.5
    ,   P.stroke "white"
    ]

drawInputLine :: forall a. Int -> Html a
drawInputLine i =
    H.line
    [   P.y1 $ 10.0 + 15.0 * Int.toNumber i
    ,   P.y2 $ 10.0 + 15.0 * Int.toNumber i
    ,   P.x1 0.0
    ,   P.x2 15.0
    ,   P.strokeWidth 0.5
    ,   P.stroke "white"
    ]

drawOutputLine :: forall a. Int -> Html a
drawOutputLine i =
    H.line
    [   P.y1 $ 20.0 + 20.0 * Int.toNumber i 
    ,   P.y2 $ 20.0 + 20.0 * Int.toNumber i
    ,   P.x1 105.0
    ,   P.x2 120.0
    ,   P.strokeWidth 0.5
    ,   P.stroke "white"
    ]    

view :: Model -> Html Msg
view model@{patterns, states, currentState, currentPattern, selectedInput, dialog} =
    H.div [H.class_ "w-full min-h-full bg-black"]
    [   H.div [H.class_ "flex flew-row"]
        [   H.div [H.class_ "w-1/4"]
            [   H.button [H.class_ buttonClass, E.onClick \_ -> OpenDialog EditorDialog]
                [   H.text "Modifier le motif"
                ]
            ,   drawPattern selectedInput (patterns !!! currentPattern)
            ]
        ,   H.div [H.class_ "w-3/4 relative"] [showNetwork]
            
        ]
    ,   case dialog of
            NoDialog -> H.empty
            EditorDialog -> showEditor model
            NeuronDialog i -> neuronDialog model i 
    ]

    where
    {neurons, values} = states !!! currentState
    showNetwork =
        H.svg [P.viewBox 0 0 150 100]
        [   H.g [] $ concat $ neurons # mapWithIndex \i neuron ->
                    case neuron of
                        Input _ -> []
                        Neuron {coeffs} ->
                            coeffs <#> \{from} -> drawLine i from
        ,   H.g [] $ drawInputLine <$> (0..5)
        ,   H.g [] $ drawOutputLine <$> (0..3)
        ,   H.g [] $ map2 neurons (values !!! currentPattern) \i neuron {value} ->
                let layer = i `div` 6
                    row = i `mod` 6
                in
                case neuron of
                    Input _ ->
                        H.g [H.style "transform" $ translate (pc 0.1) (pc $ 0.1 + Int.toNumber row * 0.15)]                        
                        [   H.circle
                            [   P.r 5.0
                            ,   E.onPointerEnter \_ -> SelectInput (Just i)
                            ,   E.onPointerLeave \_ -> SelectInput Nothing
                            ,   P.fill "white"
                            ]
                        ,   H.use [P.href $ "#neuron-" <>  show i, P.x (-3.0), P.y (-3.0), P.width "6", P.height "6", H.class_ "pointer-events-none"]
                        -- ,   H.text_ (show value) [P.x (-2.5), P.y 2.0, H.attr "font-size" "0.3rem", H.attr "pointer-events" "none"] 
                        ]
                    Neuron _ ->
                        H.g [H.style "transform" $ translate
                            (pc $ if layer == 1 then 0.4 else 0.7)
                            (pc $ if layer == 1 then
                                                  0.1 + 0.15 * Int.toNumber row
                                                else
                                                  0.2 + 0.2 * Int.toNumber row 
                            )
                        ]
                        [   H.circle
                            [   P.r 5.0
                            ,   P.fill $ case i of
                                    12 -> "#A3E635"
                                    13 -> "#60A5FA"
                                    14 -> "#EC4899"
                                    15 -> "#FACC15"
                                    _ -> "white"
                            -- ,   P.stroke $ if i == selectedNeuron then "green" else "pink"
                            ,   E.onClick \_ -> OpenDialog (NeuronDialog i)
                            ]
                        ,   H.use [P.href $ "#neuron-" <>  show i, P.x (-3.0), P.y (-3.0), P.width "6", P.height "6", H.class_ "pointer-events-none"]
                        -- ,   H.text_ (show value) [P.x (-2.5), P.y 2.0, H.attr "font-size" "0.3rem", H.attr "pointer-events" "none"]
                        -- ,   H.text_ ("≥" <> show threshold) [P.x (-2.5), P.y (-5.5), H.attr "font-size" "0.2rem"]
                        ,   H.when (layer == 2) \_ ->
                                if value > 0.0 then
                                    H.text_ "✓"
                                    [   P.x 20.0
                                    ,   H.attr "font-size" "0.4rem"
                                    ,   P.stroke "green"
                                    ]
                                else
                                    H.text_ "⨯"
                                    [   P.x 20.0
                                    ,   H.attr "font-size" "0.4rem"
                                    ,   P.stroke "red"
                                    ]
                        ]
        ]