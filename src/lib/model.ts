import { count, filterMap, range, zipWith } from "@gbagan/utils";
import { scalarProduct } from "./util";
import {
  emptyPattern, pattern01, pattern02, pattern03, pattern04, pattern31,
  pattern32, pattern33, pattern34, pattern61, pattern62, pattern63,
  pattern64, pattern91, pattern92, pattern93, pattern94
} from "./patterns";

export type Matrix = readonly (readonly number[])[];
export type Mask = readonly (readonly boolean[])[];

export type Pattern = {
  readonly symbol: number;
  readonly pattern: readonly boolean[];
  readonly selected: boolean;
}

type Output = {
  readonly hidden: readonly number[],
  readonly final: readonly number[]
}[];

export type State = {
  readonly hiddenThresholds: readonly number[];
  readonly hiddenWeights: Matrix;
  readonly finalThresholds: readonly number[];
  readonly finalWeights: Matrix;
  readonly output: Output;
  readonly iter: number;
}

export type NDialog = {
  readonly type: "neuron";
  readonly layer: number;
  readonly idx: number;
}

export type Dialog = {
  readonly type: "none" | "edit" | "neurons"
} | NDialog;

export const MASK: Mask = [
  [1, 1, 1, 0, 0, 0],
  [1, 1, 0, 0, 0, 1],
  [0, 0, 1, 1, 1, 0],
  [0, 1, 0, 1, 0, 1],
  [1, 0, 0, 0, 1, 1],
  [0, 0, 0, 1, 1, 1]
].map(row => row.map(b => b === 1));

export const patternFns: (() => Pattern)[] = [
  () => ({ symbol: 0, pattern: pattern01(), selected: true }),
  () => ({ symbol: 0, pattern: pattern02(), selected: true }),
  () => ({ symbol: 0, pattern: pattern03(), selected: true }),
  () => ({ symbol: 0, pattern: pattern04(), selected: true }),
  () => ({ symbol: 0, pattern: emptyPattern(), selected: false }),
  () => ({ symbol: 0, pattern: emptyPattern(), selected: false }),

  () => ({ symbol: 1, pattern: pattern31(), selected: true }),
  () => ({ symbol: 1, pattern: pattern32(), selected: true }),
  () => ({ symbol: 1, pattern: pattern33(), selected: true }),
  () => ({ symbol: 1, pattern: pattern34(), selected: true }),
  () => ({ symbol: 1, pattern: emptyPattern(), selected: false }),
  () => ({ symbol: 1, pattern: emptyPattern(), selected: false }),

  () => ({ symbol: 2, pattern: pattern61(), selected: true }),
  () => ({ symbol: 2, pattern: pattern62(), selected: true }),
  () => ({ symbol: 2, pattern: pattern63(), selected: true }),
  () => ({ symbol: 2, pattern: pattern64(), selected: true }),
  () => ({ symbol: 2, pattern: emptyPattern(), selected: false }),
  () => ({ symbol: 2, pattern: emptyPattern(), selected: false }),

  () => ({ symbol: 3, pattern: pattern91(), selected: true }),
  () => ({ symbol: 3, pattern: pattern92(), selected: true }),
  () => ({ symbol: 3, pattern: pattern93(), selected: true }),
  () => ({ symbol: 3, pattern: pattern94(), selected: true }),
  () => ({ symbol: 3, pattern: emptyPattern(), selected: false }),
  () => ({ symbol: 3, pattern: emptyPattern(), selected: false })
]


export const initPatterns = patternFns.map(f => f());

// compte le nombre de pixels que capte le neurone d'entrée i sur un pattern donné
export const countPixels = (i: number, pattern: readonly boolean[]) =>
  count(pattern, (b, j) => {
    const row = j / 18 | 0
    const col = (j % 6) / 2 | 0
    return b && (i < 3 ? col === i : row === i - 3);
  });

// todo
// firstAvailableHeight([0, 1, 2, 4, 6, 7]) === 3
export function firstAvailableHeight(xs: readonly number[]) {
  let i = 0;
  while(true) {
    if (!xs.includes(i)) {
      return i;
    }
    i++
  }
}

type Symbol = {
  readonly symbol: number;
  readonly x: number;
  readonly y: number;
}

type RulerPositions = {
  readonly zero: number;
  readonly symbols: Symbol[];
  readonly graduation: readonly { readonly value: number, readonly x: number}[];
}

export function rulerPositions(patterns: readonly Pattern[], st: State, layer: number, j: number): RulerPositions {
  const values = st.output.map(({ hidden, final }) => layer === 1 ? hidden[j] : final[j]);
  const minX = Math.min(...values) ?? 0; // todo Math.min returns Infinity
  const maxX = Math.max(...values) ?? 0; // todo
  const values2 = values.map(v => (v - minX) / (maxX - minX)); // todo
  
  const symbolsTmp = filterMap(patterns, ({symbol, selected}, k) =>
    selected ? { symbol, value: values2[k] } : null
  ).sort((a, b) => a.value - b.value);

  const symbols: Symbol[] = [];
  for (const { symbol, value } of symbolsTmp) {
    const previousHeights = filterMap(symbols, ({x, y}) => value - x <= 0.049 ? y : null);
    const y = firstAvailableHeight(previousHeights);
    symbols.push({ symbol, x: value, y });
  }

  return {
    zero: -minX / (maxX - minX), // todo
    symbols,
    graduation: range(Math.floor(minX), Math.ceil(maxX)+1)
      .map(value => ({ value, x: (value - minX) / (maxX - minX) }))
  }
}

// calcule la valeur des neuronnes d'entrée à partir d'une liste de patterns
export function updateInput(patterns: readonly Pattern[]): number[][] {
  return patterns.map(({ pattern }) =>
    [0, 1, 2, 3, 4, 5].map(i => countPixels(i, pattern))
  )
}

export function updateOutput(inputs: Matrix, st: State): Output {
  return inputs.map(input => {
    const hidden = zipWith(st.hiddenThresholds, st.hiddenWeights, (t, hw) =>
      Math.max(0, scalarProduct(input, hw) - t)
    );
    const final = zipWith(st.finalThresholds, st.finalWeights, (t, fw) =>
      scalarProduct(hidden, fw) - t
    );
    return { final, hidden }
  })
}

export const initState: State = {
  hiddenThresholds: [8, 7, 18, 6, 16, 5],
  hiddenWeights: [
    [-1, 1, 1, 0, 0, 0],
    [1, 1, 0, 0, 0, -1],
    [0, 0, 1, 1, 1, 0],
    [0, 1, 0, -1, 0, 1],
    [1, 0, 0, 0, 1, 1],
    [0, 0, 0, 1, -1, 1],
  ],
  finalThresholds: [7, 5, 6, 5],
  finalWeights: [
    [-1, -1, 1, -1, 1, 1],
    [1, 1, -1, 1, -1, 0],
    [-1, 1, -1, 1, 1, -1],
    [1, 1, 1, -1, -1, 0],
  ],
  output: [],
  iter: 0,
}

/*
export initModel: => Model := =>
  patterns: initPatterns()
  inputs: []
  states: [initState()]
  currentState: 0
  currentPattern: 0
  selectedInput: null
  editMode: false
  dialog: { type: "none" }

*/