import { Component, Index } from "solid-js"
import { Pattern } from "../model";
import MiniPattern from "./MiniPattern";
import { checkboxClass } from "../constants";

type EditorComponent = Component<{
  patterns: Pattern[],
  currentPattern: number,
  setCurrentPattern: (i: number) => void,
  togglePattern: (i: number) => void,
  resetPatterns: () => void,
  changePixel: (i: number) => void,
  closeDialog: () => void,
}>;

const Editor: EditorComponent = props => (
  <>
    <div class="dialogtitle">Modifier le motif</div>
    <div class="dialogbody flex flex-row items-center gap-8">
      <div class="m-2 overflow-hidden relative bg-white border-4 w-64 h-96">
          <Index each={props.patterns[props.currentPattern].pattern}>
            {(b, i) => (
              <div
                class="absolute"
                style={{
                  background: b() ? "black" : "white",
                  width: "16.66666666%",
                  height: "11.1111111111%",
                  left: `${100 * (i % 6) / 6}%`,
                  top: `${100 * (i / 6 | 0) / 9}%`,
                }}
                onClick={[props.changePixel, i]}
              />
            )}
          </Index>
      </div>
      <div class="grid grid-cols-6 gap-1">
        <Index each={props.patterns}>
          {(pattern, i) =>
            <div class="flex flex-row items-center">
              <label class="relative inline-flex items-center cursor-pointer">
                <input
                  type="checkbox"
                  checked={pattern().selected}
                  class="sr-only peer"
                  onChange={[props.togglePattern, i]}
                />
                <div class={checkboxClass} />
              </label>
              <MiniPattern
                pattern={pattern()}
                isCurrent={i === props.currentPattern}
                onClick={[props.setCurrentPattern, i]}
              />
            </div>
          }
        </Index>
      </div>
    </div>
    <div class="p-4 text-right">
      <button class="btn" onClick={props.resetPatterns}>Réinitialiser</button>
      <button class="btn ml-4" onClick={props.closeDialog}>OK</button>
    </div>
  </>
)

export default Editor;