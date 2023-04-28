module Neuron.View (view) where

import Prelude

import Data.Array ((..), (!!), concat, filter, length, mapWithIndex, intersperse)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number.Format (toStringWith, fixed)
import Neuron.Model (Dialog(..), Model, Pattern, RulerPositions, mask, rulerPositions)
import Neuron.Msg (Msg(..))
import Neuron.Util ((!))
import Neuron.UI as UI
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc, px, translate)

patternColor :: Int -> String
patternColor = case _ of
  0 -> "#A3E635"
  1 -> "#60A5FA"
  2 -> "#EC4899"
  _ -> "#FACC15"

showNeuron ∷ ∀msg. Int → Html msg
showNeuron k =
  H.div [ H.class_ "inline-block w-8 h-8" ]
    [ H.svg [ H.class_ "w-full h-full" ]
        [ H.use [ P.href $ "#neuron-" <> show k ]]
    ]

drawRulerSymbol :: ∀msg. Number -> Number -> Int -> Html msg
drawRulerSymbol x y color =
  H.g [H.style "transform" $ translate (px $ x * 100.0) (px $ 25.0 - y * 5.0)]
    [ H.rect [P.x (-2.5), P.y 0.0, P.width "5", P.height "5", P.fill (patternColor color)]
    , H.text_ (show $ color * 3) [P.x (-1.0), P.y (4.5), H.attr "font-size" "0.3rem"] 
    ]

drawRuler :: ∀msg. Boolean -> RulerPositions -> Html msg
drawRuler small res =
    H.div [H.class_ $ "border " <> if small then "w-[40vw]" else  "w-[75vw]"]
    [ H.svg [P.viewBox (-10) 0 120 30]
        [ H.rect [P.x (-10.0), P.y 0.0, P.width "120", P.height "40", P.fill "#B0FFB0"]
        , H.rect [P.x (-10.0), P.y 0.0, P.width $ show $ 7.5 + res.zero * 100.0, P.height "40", P.fill "#FFB0B0"]
        , H.g [] $ res.graduation <#> \{x} ->
            H.line [ P.x1 (x*100.0-2.5), P.x2 (x*100.0-2.5), P.y1 0.0, P.y1 30.0
                   , P.strokeWidth 0.2, P.stroke "#808080", P.strokeDasharray "0.5"] 
        , H.g [] $
            res.symbols <#> \{symbol, x, y} -> drawRulerSymbol x y symbol
        ]
    ]

showEditor :: Model -> Html Msg
showEditor { patterns, currentPattern } = UI.dialog "Modifier le motif" [body] buttons
  where
  body =
    H.div [ H.class_ "flex flex-row items-center gap-8" ]
      [ editor
      , H.div [ H.class_ "grid grid-cols-6 gap-2" ] $
          patterns # mapWithIndex \i pattern ->
            H.div [H.class_ "flex flex-col items-center"]
            [ H.label [H.class_ "relative inline-flex items-center cursor-pointer"]
              [ H.input
                [ P.type_ "checkbox"
                , P.checked pattern.selected
                , H.class_ "sr-only peer"
                , E.onChecked \_ -> TogglePattern i
                ]
              , H.div [H.class_ UI.checkboxClass] []  
              ]
            , drawMiniPattern i (i == currentPattern) pattern
            ]
      ]

  editor = H.maybe (patterns !! currentPattern) \{ pattern } ->
    H.div [ H.class_ "m-2 overflow-hidden relative border-4 w-64 h-96" ] $
      pattern # mapWithIndex \i b ->
        let
          row = i `div` 6
          col = i `mod` 6
        in
          H.div
            [ H.class_ "absolute pattern-pixel"
            , H.class' "black" b
            , H.style "width" "16.66666666%"
            , H.style "height" "11.1111111111%"
            , H.style "left" $ pc $ Int.toNumber col / 6.0
            , H.style "top" $ pc $ Int.toNumber row / 9.0
            , E.onClick \_ -> ChangePixel i
            ]
            []
  buttons =
    [ { name: "Réinitialiser", onClick: ResetPattern }
    , { name: "Ok", onClick: OpenDialog NoDialog }
    ]

neuronDialog :: Model -> Int -> Int -> Html Msg
neuronDialog { editMode, states, currentState, patterns } i j = UI.dialog title [body] buttons
  where
  body = H.div [H.class_ "flex flex-col"]
    [ drawCalculus
    , ruler
    , H.div [H.class_ "text-4xl"]
      [ H.text $ "Nombre d'itérations: " <> show st.iter
      , H.when (length states > 1) \_ ->
          H.input [ H.class_ "h-2 bg-gray-700 rounded-lg appearance-none cursor-pointer"
                  , P.type_ "range", P.min 0, P.max (length states - 1)
                  , P.value (show currentState)
                  , E.onValueChange ChangeCurrentState
                  ]
      ]
    ]

  st = states ! currentState
  threshold = if i == 1 then st.hiddenThresholds ! j else st.finalThresholds ! j
  weights = if i == 1 then st.hiddenWeights ! j else st.finalWeights ! j 

  title = "Neurone " <> case i, j of
    1, 0 -> "A"
    1, 1 -> "B"
    1, 2 -> "C"
    1, 3 -> "D"
    1, 4 -> "E"
    1, 5 -> "F"
    2, 0 -> "vert"
    2, 1 -> "bleu"
    2, 2 -> "rose"
    _, _ -> "jaune"

  drawCalculus = H.div [ H.class_ "text-4xl" ] $
    [ if editMode then
        H.span [] $ intersperse (H.span [ H.class_ "text-4xl mx-4" ] [ H.text "+" ]) $
          weights # mapWithIndex \k weight ->
            H.span []
              [ H.input
                  [ H.class_ UI.inputNumberClass
                  , P.type_ "number"
                  , P.value (show weight)
                  , E.onValueChange $ ChangeWeight i j k
                  ]
              , showNeuron $ (if i == 2 then 6 else 0) + k
              ]
      else
        H.span [] $ concat $
            weights
                # mapWithIndex (\index weight -> {index, weight})
                # filter (\{weight} -> weight /= 0.0)
                # mapWithIndex \k {index, weight} ->
                    [ H.text $ 
                        (if k > 0 && weight > 0.0 then "+" else "") <> 
                        (if weight == 1.0 then ""
                         else if weight == -1.0 then "-"
                         else toStringWith (fixed 3) weight
                        )
                    , showNeuron $ (if i == 2 then 6 else 0) + index
                    ]
    , H.br
    , H.text "Seuil: "
    , if editMode then
        H.input
          [ H.class_ UI.inputNumberClass
          , P.type_ "number"
          , P.value (show threshold)
          , E.onValueChange $ ChangeThreshold i j
          ]
      else
        H.text (show threshold)
    ]

  ruler = drawRuler false $ rulerPositions patterns st i j

  buttons =
    [ { name: "Editer", onClick: ToggleEditMode }
    , { name: "Apprendre", onClick: RunLearning }
    , { name: "Ok", onClick: OpenDialog NoDialog }
    ]

allNeuronsDialog :: Model -> Html Msg
allNeuronsDialog { states, currentState, patterns } = UI.dialog "Neurones de sortie" [body] buttons
  where
  body = H.div [H.class_ "flex flex-col"]
    [ H.div [H.class_ "grid grid-cols-2 gap-8"] $
        [0, 1, 2, 3] <#> drawRuler true <<< rulerPositions patterns st 2
    , H.div [H.class_ "h-24 mt-12 text-4xl grid grid-cols-2 gap-8"]
      [ H.text $ "Nombre d'itérations: " <> show st.iter
      , H.when (length states > 1) \_ ->
          H.div []
            [ UI.button "Lancer" RunSimulation
            , H.input [ H.class_ "h-2 bg-gray-700 rounded-lg appearance-none cursor-pointer"
                  , P.type_ "range", P.min 0, P.max (length states - 1)
                  , P.value (show currentState)
                  , E.onValueChange ChangeCurrentState
                  ]
            ]
      ]
    ]

  st = states ! currentState

  buttons =
    [ { name: "Apprendre", onClick: RunLearning }
    , { name: "Ok", onClick: OpenDialog NoDialog }
    ]

drawPattern :: Maybe Int -> Pattern -> Html Msg
drawPattern selectedCaptor { pattern } =
  H.div [ H.class_ "overflow-hidden relative border-4 border-slate-400 m-2 w-64 h-96"
        , E.onClick \_ -> OpenDialog EditorDialog
        ] $
    pattern # mapWithIndex \i b ->
      let
        row = i `div` 6
        col = i `mod` 6
        capt = Just (row `div` 3 + 3) == selectedCaptor || Just (col `div` 2) == selectedCaptor
      in
        H.div
          [ H.class_ "absolute pointer-events-none pattern-pixel"
          , H.class' "black" b
          , H.class' "selected" capt
          , H.style "width" "16.66666666%"
          , H.style "height" "11.1111111111%"
          , H.style "left" $ pc $ Int.toNumber col / 6.0
          , H.style "top" $ pc $ Int.toNumber row / 9.0
          ]
          []

drawMiniPattern :: Int -> Boolean -> Pattern -> Html Msg
drawMiniPattern index isCurrent { symbol, pattern } =
  H.div
    [ H.class_ "overflow-hidden relative border-4 border-slate-400 m-2 w-24 h-36"
    , H.class_ if isCurrent then "border-green-500" else "border-slate-400"
    , E.onClick \_ -> SelectPattern index
    ] $
    pattern # mapWithIndex \i b ->
      let
        row = i `div` 6
        col = i `mod` 6
        color = case symbol, b of
          _, false -> "bg-white"
          0, _ -> "bg-green-400"
          1, _ -> "bg-blue-400"
          2, _ -> "bg-pink-500"
          _, _ -> "bg-yellow-400"
      in
        H.div
          [ H.class_ $ "absolute " <> color
          , H.style "width" "16.66666666%"
          , H.style "height" "11.1111111111%"
          , H.style "left" $ pc $ Int.toNumber col / 6.0
          , H.style "top" $ pc $ Int.toNumber row / 9.0
          ]
          []

drawLine :: forall a. Int -> Int -> Int -> Int -> Html a
drawLine layer row layer' row' =
  H.line
    [ P.y1 $ if layer <= 1 then 10.0 + 15.0 * Int.toNumber row else 20.0 + 20.0 * Int.toNumber row
    , P.y2 $ if layer' <= 1 then 10.0 + 15.0 * Int.toNumber row' else 20.0 + 20.0 * Int.toNumber row'
    , P.x1 $ if layer == 0 then 15.0 else if layer == 1 then 60.0 else 105.0
    , P.x2 $ if layer' == 0 then 15.0 else if layer' == 1 then 60.0 else 105.0
    , P.strokeWidth 0.5
    , P.stroke "white"
    ]

drawInputLine :: forall a. Int -> Html a
drawInputLine i =
  H.line
    [ P.y1 $ 10.0 + 15.0 * Int.toNumber i
    , P.y2 $ 10.0 + 15.0 * Int.toNumber i
    , P.x1 0.0
    , P.x2 15.0
    , P.strokeWidth 0.5
    , P.stroke "white"
    ]

drawOutputLine :: forall a. Int -> Html a
drawOutputLine i =
  H.line
    [ P.y1 $ 20.0 + 20.0 * Int.toNumber i
    , P.y2 $ 20.0 + 20.0 * Int.toNumber i
    , P.x1 105.0
    , P.x2 120.0
    , P.strokeWidth 0.5
    , P.stroke "white"
    ]

drawNeuron :: forall a. Int -> Html a
drawNeuron i =
  H.use
    [ P.href $ "#neuron-" <> show i
    , P.x (-3.0)
    , P.y (-3.0)
    , P.width "6"
    , P.height "6"
    , H.class_ "pointer-events-none"
    ]

view :: Model -> Html Msg
view model@{ patterns, states, currentState, currentPattern, selectedInput, dialog } =
  H.div [ H.class_ "w-full min-h-full bg-black" ]
    [ H.div [ H.class_ "flex flew-row" ]
        [ H.div [ H.class_ "w-1/4" ]
            [ drawPattern selectedInput (patterns ! currentPattern)
            , UI.button "Afficher les réglettes" (OpenDialog AllNeuronsDialog) 
            , UI.button "Réinitialiser" Reset  
            ]
        , H.div [ H.class_ "w-3/4 relative" ] [ showNetwork ]
        ]
    , case dialog of
        NoDialog -> H.empty
        EditorDialog -> showEditor model
        NeuronDialog i j -> neuronDialog model i j
        AllNeuronsDialog -> allNeuronsDialog model
    ]

  where
  { output } = states ! currentState
  { final } = output ! currentPattern
  showNetwork =
    H.svg [ P.viewBox 0 0 150 100 ]
      [ H.g [] $ concat $ mask # mapWithIndex \i -> mapWithIndex \j v ->
          H.when v \_ -> drawLine 0 j 1 i
      , H.g [] $ do
          i <- 0 .. 5
          j <- 0 .. 3
          pure $ drawLine 1 i 2 j
      , H.g [] $ drawInputLine <$> 0 .. 5
      , H.g [] $ drawOutputLine <$> 0 .. 3
      , H.g [] $ 0 .. 5 <#> \i ->
          H.g [ H.style "transform" $ translate (pc 0.1) (pc $ 0.1 + 0.15 * Int.toNumber i) ]
            [ H.circle
                [ P.r 5.0
                , E.onPointerEnter \_ -> SelectInput (Just i)
                , E.onPointerLeave \_ -> SelectInput Nothing
                , P.fill "white"
                ]
            , drawNeuron i
            -- ,   H.text_ (show value) [P.x (-2.5), P.y 2.0, H.attr "font-size" "0.3rem", H.attr "pointer-events" "none"] 
            ]
      , H.g [] $ (0 .. 5) <#> \i ->
          H.g [ H.style "transform" $ translate (pc 0.4) (pc $ 0.1 + 0.15 * Int.toNumber i) ]
            [ H.circle
                [ P.r 5.0
                , P.fill "white"
                , E.onClick \_ -> OpenDialog (NeuronDialog 1 i)
                ]
            , drawNeuron (6 + i)
            ]
      , H.g [] $ final # mapWithIndex \i value ->
          H.g [ H.style "transform" $ translate (pc 0.7) (pc $ 0.2 + 0.2 * Int.toNumber i) ]
            [ H.circle
                [ P.r 5.0
                , P.fill $ patternColor i
                , E.onClick \_ -> OpenDialog (NeuronDialog 2 i)
                ]
            , if value > 0.0 then
                H.text_ "✓"
                  [ P.x 20.0
                  , H.attr "font-size" "0.4rem"
                  , P.stroke "green"
                  ]
              else
                H.text_ "⨯"
                  [ P.x 20.0
                  , H.attr "font-size" "0.4rem"
                  , P.stroke "red"
                  ]
            ]
      ]