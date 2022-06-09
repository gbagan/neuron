module Neuron.View
  ( view
  )
  where

import Prelude

import Data.Array ((..), (!!), concat, mapWithIndex)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Neuron.Model (Pattern, Model, Msg(..), Neuron(..), thresholdRuleToString)
import Neuron.Util (map2)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc, translate)
import Web.Event.Event (Event)

foreign import slStringValue ∷ Event → Effect String

showEditor :: Model -> Html Msg
showEditor {patterns, selectedPattern} =
    H.div [H.class_ "absolute flex-center ui-dialog-container"]
    [   H.div [H.class_ "ui-dialog"]
        [   H.div [H.class_ "ui-dialog-head"]
            [   H.div [H.class_ "ui-dialog-title"] [H.text "Modifier le motif"]
            ]
        ,   H.div [H.class_ "ui-dialog-body"] [body]
        ,   H.div [H.class_ "ui-dialog-buttons"] 
            [   H.elem "sl-button" 
                [   E.onClick \_ -> ResetPattern
                ]
                [   H.text "Réinitialiser"
                ]
            ,   H.elem "sl-button"
                [   E.onClick \_ -> OpenPatternEditor false
                ]
                [   H.text "Ok"
                ]
            ]
        ]
    ]
    where
    body = H.maybe (patterns !! selectedPattern) \pattern ->
        H.div [H.class_ "pattern-editor"] $
            pattern # mapWithIndex \i b ->
                let row = i `div` 9
                    col = i `mod` 9
                in
                H.div
                [   H.class_ "pattern-pixel" 
                ,   H.class' "black" b
                ,   H.style "width" "11.1111111111%"
                ,   H.style "height" "11.1111111111%"
                ,   H.style "left" $ pc $ Int.toNumber col / 9.0
                ,   H.style "top" $ pc $ Int.toNumber row / 9.0
                ,   E.onClick \_ -> ChangePixel i
                ] []


drawPattern :: Int -> Boolean -> Maybe Int -> Pattern -> Html Msg
drawPattern index selected selectedCaptor  bmap =
    H.div
    [   H.class_ "pattern"
    ,   H.class' "selected" selected
    ,   E.onClick \_ -> SelectPattern index
    ] $
        bmap # mapWithIndex \i b->
            let row = i `div` 9
                col = i `mod` 9
                capt = selected && (Just (row `div` 3) == selectedCaptor || Just (col `div` 3 + 3) == selectedCaptor) 
            in
            H.div
            [   H.class_ "pattern-pixel"
            ,   H.class' "black" b
            ,   H.class' "selected" capt
            ,   H.style "width" "11.1111111111%"
            ,   H.style "height" "11.1111111111%"
            ,   H.style "left" $ pc $ Int.toNumber col / 9.0
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
    ,   P.stroke "grey"
    ]

drawInputLine :: forall a. Int -> Html a
drawInputLine i =
    H.line
    [   P.y1 $ 10.0 + 15.0 * Int.toNumber i
    ,   P.y2 $ 10.0 + 15.0 * Int.toNumber i
    ,   P.x1 0.0
    ,   P.x2 15.0
    ,   P.strokeWidth 0.5
    ,   P.stroke "grey"
    ]

drawOutputLine :: forall a. Int -> Html a
drawOutputLine i =
    H.line
    [   P.y1 $ 20.0 + 20.0 * Int.toNumber i 
    ,   P.y2 $ 20.0 + 20.0 * Int.toNumber i
    ,   P.x1 105.0
    ,   P.x2 120.0
    ,   P.strokeWidth 0.5
    ,   P.stroke "grey"
    ]    

view :: Model -> Html Msg
view model@{patterns, neurons, selectedPattern, selectedInput, selectedNeuron, values, editorOpen, thresholdRule} =
    H.div [H.class_ "layout"]
    [   H.div [H.class_ "row"]
        [   H.div [H.class_ "col-6"]
            [   H.elem "sl-button" [E.onClick \_ -> OpenPatternEditor true]
                [   H.text "Modifier le motif"
                ]
            ,   H.div [] $
                    patterns # mapWithIndex \i -> drawPattern i (selectedPattern == i) selectedInput
            ,   H.maybe (neurons !! selectedNeuron) case _ of
                    Input _ -> H.empty
                    Neuron {coeffs, threshold} ->
                        H.div [H.class_ "config-main"] $
                        [   H.div [] [ H.text "Règle pour le seuil" ]
                        ,   H.elem "sl-select"
                            [   P.value (thresholdRuleToString thresholdRule)
                            ,   E.on "sl-change" \ev → Just <$> SetThresholdRule <$> slStringValue ev
                            ]
                            [   H.elem "sl-menu-item" [ P.value "standard" ] [ H.text "Standard" ]
                            ,   H.elem "sl-menu-item" [ P.value "bool" ] [ H.text "Booléen" ]
                            ]
                        ,   H.div [] [H.text $ "Seuil: "]
                        ,   H.elem "sl-input"
                            [   P.type_ "number"
                            ,   P.value (show threshold)
                            ,   P.min 0
                            ,   E.on "sl-change" \ev → Just <$> ChangeThreshold <$> slStringValue ev
                            ] []
                        ] <> (concat $ coeffs # mapWithIndex \i {coeff} ->
                            [   H.div [] [H.text $ "Coeff " <> show (i + 1) <> ": "]
                            ,   H.elem "sl-input"
                                [   P.type_ "number"
                                ,   P.value (show coeff)
                                ,   P.min (-9)
                                ,   P.max 9
                                ,   E.on "sl-change" \ev → Just <$> ChangeCoeff i <$> slStringValue ev
                                ] []
                            ]
                        )
            ]
        ,   H.div [H.class_ "col-24 relative"] [showNetwork]
            
        ]
    ,   H.when editorOpen \_ -> showEditor model
    ]

    where
    showNetwork =
        H.svg [P.viewBox 0 0 150 100]
        [   H.g [] $ concat $ neurons # mapWithIndex \i neuron ->
                    case neuron of
                        Input _ -> []
                        Neuron {coeffs} ->
                            coeffs <#> \{from} -> drawLine i from
        ,   H.g [] $ drawInputLine <$> (0..5)
        ,   H.g [] $ drawOutputLine <$> (0..3)
        ,   H.g [] $ map2 neurons values \i neuron value ->
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
                            ,   P.fill "pink"
                            ]
                        ,   H.text_ (show value) [P.x (-2.5), P.y 2.0, H.attr "font-size" "0.3rem", H.attr "pointer-events" "none"] 
                        ]
                    Neuron {threshold, coeffs} ->
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
                            ,   P.fill "pink"
                            ,   P.stroke $ if i == selectedNeuron then "green" else "pink"
                            ,   E.onClick \_ -> SelectNeuron i
                            ]
                        ,   H.text_ (show value) [P.x (-2.5), P.y 2.0, H.attr "font-size" "0.3rem", H.attr "pointer-events" "none"]
                        ,   H.text_ ("≥" <> show threshold) [P.x (-2.5), P.y (-5.5), H.attr "font-size" "0.2rem"]
                        ,   H.g [] $
                                coeffs # mapWithIndex \j {coeff} ->
                                    H.text_ ("×" <> show coeff) 
                                    [   P.x (-8.0)
                                    ,   P.y (Int.toNumber j * 3.0)
                                    ,   H.attr "font-size" "0.18rem"
                                    ,   H.attr "pointer-events" "none"
                                    ]
                        ,   H.when (layer == 2) \_ ->
                                if value >= threshold then
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