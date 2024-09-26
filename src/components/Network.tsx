import { Component, Index } from "solid-js";
import { mask } from "../model";
import range from "lodash.range";
import { patternColors } from "../constants";

type LineComponent = Component<{
  layer1: number,
  layer2: number,
  row1: number,
  row2: number,
}>

const Line: LineComponent = props => (
  <line
    y1={props.layer1 <= 1 ? 10 + 15 * props.row1 : 20 + 20 * props.row1}
    y2={props.layer2 <= 1 ? 10 + 15 * props.row2 : 20 + 20 * props.row2}
    x1={props.layer1 === 0 ? 15 : props.layer1 === 1 ? 60 : 105}
    x2={props.layer2 === 0 ? 15 : props.layer2 === 1 ? 60 : 105}
    stroke-width="0.5"
    stroke="white"
  />
)

type NetworkComponent = Component<{
    final: number[],
    selectInput: (i: number | null) => void,
}>

const Network: NetworkComponent = props => (
  <svg viewBox="0 0 150 100">
    {mask.map((row, i) =>
      <>
        {row.map((v, j) => 
          v && <Line layer1={0} layer2={1} row1={j} row2={i} /> 
        )}
      </>
    )}
    {range(0, 6).map(i =>
      <>
        {range(0, 4).map(j =>
          <Line layer1={1} layer2={2} row1={i} row2={j} />
        )}
      </>
    )}
    {range(0, 6).map(i =>
      <line
        y1={10 + 15 * i}
        y2={10 + 15 * i}
        x1="0"
        x2="15"
        stroke-width="0.5"
        stroke="white"
      />
    )}
    {range(0, 4).map(i =>
      <line
        y1={20 + 20 * i}
        y2={20 + 20 * i}
        x1="105"
        x2="120"
        stroke-width="0.5"
        stroke="white"
      />
    )}
    {range(0, 6).map(i =>
      <g
        style={{transform: `translate(10%, ${10+15*i}%)`}}
      >
        <circle
          r="5"
          fill="white"
          onPointerEnter={() => props.selectInput(i)}
          onPointerLeave={() => props.selectInput(null)}
        />
        <use
          href={`#neuron-${i}`}
          x="-3.0"
          y="-3.0"
          width="6"
          height="6"
          class="pointer-events-none"
        />
      </g>
    )}
    {range(0, 6).map(i =>
      <g
        style={{transform: `translate(40%, ${10+15*i}%)`}}
      >
        <circle
          r="5"
          fill="white"
        />
        <use
          href={`#neuron-${6+i}`}
          x="-3.0"
          y="-3.0"
          width="6"
          height="6"
          class="pointer-events-none"
        />
      </g>
    )}
    <Index each={props.final}>
      {(value, i) =>
        <g style={{transform: `translate(70%, ${20 + 20 * i}%)`}}>
          <circle
            r="5"
            fill={patternColors[i]}
          />
          <text
            x="20"
            stroke={value() > 0 ? "green" : "red"}
            font-size="0.4rem"
          >
            {value() > 0 ? "✓" : "⨯"}
          </text>
        </g>
      }
    </Index>
  </svg>
)
  /*
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
            */

export default Network;