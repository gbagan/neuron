import { Component, Index } from "solid-js"
import { Pattern } from "../model"

const colors = ["white", "black", "lightgreen", "darkgreen"];

type PatternComponent = Component<{
  selectedCaptor: number | null,
  pattern: Pattern,
  onClick: () => void,
}>

const PatternView: PatternComponent = props => (
  <div
    class="overflow-hidden relative border-4 border-slate-400 m-2 w-[30vmin] h-[45vmin] cursor-pointer"
    onClick={props.onClick}
  >
    <Index each={props.pattern.pattern}>
      {((b, i) => {
        const row = i / 6 | 0;
        const col = i % 6;
        const list: any[] = [(row / 3 | 0) + 3, col / 2 | 0];
        return (
          <div
            class="absolute pointer-events-none"
            style={{
              background: colors[Number(list.includes(props.selectedCaptor)) * 2 + Number(b())],
              width: "16.66666666%",
              height: "11.1111111111%",
              left: `${col * 100 / 6}%`,
              top: `${row * 100 / 9}%`,
            }}
          />
        )
      })}
    </Index>
  </div>
);

export default PatternView;