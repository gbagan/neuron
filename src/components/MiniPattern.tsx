import { Component, Index, JSX } from "solid-js"
import { Pattern } from "../model"

const colors =  ["bg-green-400", "bg-blue-400", "bg-pink-500", "bg-yellow-400"]

type MiniPatternComponent = Component<{
  pattern: Pattern,
  isCurrent: boolean,
  onClick: JSX.EventHandlerUnion<HTMLDivElement, MouseEvent>
}>

const MiniPattern: MiniPatternComponent = props =>
  <div
    class="overflow-hidden relative bg-white border-4 border-slate-400 m-2 w-24 h-36"
    classList={{
      "border-green-500": props.isCurrent,  
      "border-slate-400": !props.isCurrent,
    }}
    onClick={props.onClick}
  >
   <Index each={props.pattern.pattern}>
    {((b, i) =>
      <div
        class={"absolute " + (b() ? colors[props.pattern.symbol] : "bg-white")}
        style={{
          width: "16.66666666%",
          height: "11.1111111111%",
          left: `${(i % 6) * 100 / 6}%`,
          top: `${(i / 6 | 0) * 100  / 9}%`,
        }}
      />
    )}
   </Index>
  </div>

export default MiniPattern;