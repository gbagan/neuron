import { Component, Index } from "solid-js";
import { mask } from "../model";
import { patternColors } from "../constants";
import Neuron from "./Neuron";

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
    openNeuronDialog: (layer: number, idx: number) => void,
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
    {[0, 1, 2, 3, 4, 5].map(i =>
      <>
        {[0, 1, 2, 3].map(j =>
          <Line layer1={1} layer2={2} row1={i} row2={j} />
        )}
      </>
    )}
    {[0, 1, 2, 3, 4, 5].map(i =>
      <line
        y1={10 + 15 * i}
        y2={10 + 15 * i}
        x1="0"
        x2="15"
        stroke-width="0.5"
        stroke="white"
      />
    )}
    {[0, 1, 2, 3].map(i =>
      <line
        y1={20 + 20 * i}
        y2={20 + 20 * i}
        x1="105"
        x2="120"
        stroke-width="0.5"
        stroke="white"
      />
    )}
    {[0, 1, 2, 3, 4, 5].map(i =>
      <g style={{transform: `translate(10%, ${10+15*i}%)`}}>
        <circle
          r="5"
          fill="white"
          onPointerEnter={() => props.selectInput(i)}
          onPointerLeave={() => props.selectInput(null)}
        />
        <Neuron idx={i}/>
      </g>
    )}
    {[0, 1, 2, 3, 4, 5].map(i =>
      <g style={{transform: `translate(40%, ${10+15*i}%)`}}>
        <circle
          r="5"
          fill="white"
          class="cursor-pointer"
          onClick={() => props.openNeuronDialog(1, i)}
        />
        <Neuron idx={i+6}/>
      </g>
    )}
    <Index each={props.final}>
      {(value, i) =>
        <g style={{transform: `translate(70%, ${20 + 20 * i}%)`}}>
          <circle
            r="5"
            fill={patternColors[i]}
            class="cursor-pointer"
            onClick={() => props.openNeuronDialog(2, i)}
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

export default Network;