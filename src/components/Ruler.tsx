import { Component, createMemo, For, Index } from "solid-js";
import { Pattern, rulerPositions, State } from "../model";
import { patternColors } from "../constants";

type RulerComponent = Component<{
  patterns: Pattern[],
  state: State,
  layer: number,
  idx: number,
  small: boolean,
}>

const Ruler: RulerComponent = props => {
  const positions = createMemo(() => rulerPositions(props.patterns, props.state, props.layer, props.idx));

  return (
    <div
      class="border"
      classList={{"w-[40vw]": props.small, "w-[75vw]": !props.small}}
    >
      <svg viewBox="-10 0 120 30">
        <rect x="-10" y="0" width="120" height="40" fill="#B0FFB0"/>
        <rect x="-10" y="0" width={7.5 + positions().zero * 100} height="40" fill="#FFB0B0" />
        <For each={positions().graduation}>
          {p => (
            <line
              x1={p.x*100-2.5}
              x2={p.x*100-2.5}
              y1="0"
              y2="30"
              stroke-width="0.2"
              stroke="#808080"
              stroke-dasharray="0.5"
            />
          )}
        </For>
        <Index each={positions().symbols}>
          {s => (
            <g style={{transform: `translate(${s().x * 100.0}px, ${25 - s().y * 5}px)`}}>
              <rect x="-2.5" y="0" width="5" height="5" fill={patternColors[s().symbol]} />
              <text x="-1" y="4.5" font-size="0.3rem">{s().symbol * 3}</text>
            </g> 
          )} 
        </Index>
     </svg>
    </div>
  );
}

export default Ruler;