module Neuron.View where

import Prelude

import Data.Array ((!!), concat, mapWithIndex)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Neuron.Model (Drawing, Model, Msg(..), Neuron(..))
import Neuron.Util (map2)
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc, translate)

showEditor :: Model -> Html Msg
showEditor {drawings, selectedDrawing} =
    H.div [H.class_ "absolute flex-center ui-dialog-container"]
    [   H.div [H.class_ "ui-dialog"]
        [   H.div [H.class_ "ui-dialog-head"]
            [   H.div [H.class_ "ui-dialog-title"] [H.text "Modifier le motif"]
            ]
        ,   H.div [H.class_ "ui-dialog-body"] [body]
        ,   H.div [H.class_ "ui-dialog-buttons"] 
            [   H.button 
                [   H.class_ "ui-button ui-button-primary"
                ,   E.onClick \_ -> ResetDrawing
                ]
                [   H.text "Réinitialiser"
                ]
            ,   H.button
                [   H.class_ "ui-button ui-button-primary"
                ,   E.onClick \_ -> OpenDrawingEditor false
                ]
                [   H.text "Ok"
                ]
            ]
        ]
    ]
    where
    body = H.maybe (drawings !! selectedDrawing) \drawing ->
        H.div [H.class_ "drawing-editor"] $
            drawing # mapWithIndex \i b ->
                let row = i `div` 9
                    col = i `mod` 9
                in
                H.div
                [   H.class_ "drawing-pixel" 
                ,   H.class' "black" b
                ,   H.style "width" "11.1111111111%"
                ,   H.style "height" "11.1111111111%"
                ,   H.style "left" $ pc $ Int.toNumber col / 9.0
                ,   H.style "top" $ pc $ Int.toNumber row / 9.0
                ,   E.onClick \_ -> ChangePixel i
                ] []




drawDrawing :: Int -> Boolean -> Maybe Int -> Drawing -> Html Msg
drawDrawing index selected selectedCaptor  bmap =
    H.div
    [   H.class_ "drawing"
    ,   H.class' "selected" selected
    ,   E.onClick \_ -> SelectDrawing index
    ] $
        bmap # mapWithIndex \i b->
            let row = i `div` 9
                col = i `mod` 9
                capt = selected && (Just (row `div` 3) == selectedCaptor || Just (col `div` 3 + 3) == selectedCaptor) 
            in
            H.div
            [   H.class_ "drawing-pixel"
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
    ,   P.stroke "black"
    ]

view :: Model -> Html Msg
view model@{drawings, neurons, selectedDrawing, selectedCaptor, selectedNeuron, values, editorOpen} =
    H.div [H.class_ "layout"]
    [   H.div [H.class_ "row"]
        [   H.div [H.class_ "col-6"]
            [   H.button [E.onClick \_ -> OpenDrawingEditor true] [H.text "Modifier le motif"]
            ,   H.div [] $
                    drawings # mapWithIndex \i -> drawDrawing i (selectedDrawing == i) selectedCaptor
            ,   H.maybe (neurons !! selectedNeuron) case _ of
                    Captor _ -> H.empty
                    From {coeffs, threshold} ->
                        H.div [] $
                        [   H.text $ "Seuil: "
                        ,   H.input [P.type_ "number", P.value (show threshold), E.onValueChange ChangeThreshold]
                        ,   H.br
                        ] <> (concat $ coeffs # mapWithIndex \i {coeff} ->
                            [   H.text $ "Coeff " <> show (i + 1) <> ": "
                            ,   H.input [P.type_ "number", P.value (show coeff), E.onValueChange (ChangeCoeff i)]
                            ,   H.br
                            ]
                        )
            ]
        ,   H.div [H.class_ "col-24 relative"]
            [   H.svg [P.viewBox 0 0 150 100]
                [   H.g [] $ concat $ neurons # mapWithIndex \i neuron ->
                            case neuron of
                                Captor _ -> []
                                From {coeffs} ->
                                    coeffs <#> \{from} -> drawLine i from
                ,   H.g [] $ map2 neurons values \i neuron value ->
                        let layer = i `div` 6
                            row = i `mod` 6
                        in
                        case neuron of
                            Captor _ ->
                                H.g [H.style "transform" $ translate (pc 0.1) (pc $ 0.1 + Int.toNumber row * 0.15)]                        
                                [   H.circle
                                    [   P.r 5.0
                                    ,   E.onPointerEnter \_ -> SelectCaptor (Just i)
                                    ,   E.onPointerLeave \_ -> SelectCaptor Nothing
                                    ,   P.fill "pink"
                                    ]
                                ,   H.text_ (show value) [P.x (-2.5), P.y 2.0, H.attr "font-size" "0.3rem", H.attr "pointer-events" "none"] 
                                ]
                            From {threshold} ->
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
                                ]
                ]
            ]
        ]
    ,   H.when editorOpen \_ -> showEditor model
    ]