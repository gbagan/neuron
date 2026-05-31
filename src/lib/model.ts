import { count, filterMap, range, zipWith } from "@gbagan/utils";
import { scalarProduct } from "./util";
import {
  emptyPattern, PATTERN01, PATTERN02, PATTERN03, PATTERN04, PATTERN31,
  PATTERN32, PATTERN33, PATTERN34, PATTERN61, PATTERN62, PATTERN63,
  PATTERN64, PATTERN91, PATTERN92, PATTERN93, PATTERN94
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

export const initPatterns: Pattern[] = [
  { symbol: 0, pattern: PATTERN01, selected: true },
  { symbol: 0, pattern: PATTERN02, selected: true },
  { symbol: 0, pattern: PATTERN03, selected: true },
  { symbol: 0, pattern: PATTERN04, selected: true },
  { symbol: 0, pattern: emptyPattern, selected: false },
  { symbol: 0, pattern: emptyPattern, selected: false },

  { symbol: 1, pattern: PATTERN31, selected: true },
  { symbol: 1, pattern: PATTERN32, selected: true },
  { symbol: 1, pattern: PATTERN33, selected: true },
  { symbol: 1, pattern: PATTERN34, selected: true },
  { symbol: 1, pattern: emptyPattern, selected: false },
  { symbol: 1, pattern: emptyPattern, selected: false },

  { symbol: 2, pattern: PATTERN61, selected: true },
  { symbol: 2, pattern: PATTERN62, selected: true },
  { symbol: 2, pattern: PATTERN63, selected: true },
  { symbol: 2, pattern: PATTERN64, selected: true },
  { symbol: 2, pattern: emptyPattern, selected: false },
  { symbol: 2, pattern: emptyPattern, selected: false },

  { symbol: 3, pattern: PATTERN91, selected: true },
  { symbol: 3, pattern: PATTERN92, selected: true },
  { symbol: 3, pattern: PATTERN93, selected: true },
  { symbol: 3, pattern: PATTERN94, selected: true },
  { symbol: 3, pattern: emptyPattern, selected: false },
  { symbol: 3, pattern: emptyPattern, selected: false }
]

// compte le nombre de pixels que capte le neurone d'entrée i sur un pattern donné
export const countPixels = (i: number, pattern: readonly boolean[]) =>
  count(pattern, (b, j) => {
    const row = j / 18 | 0
    const col = (j % 6) / 2 | 0
    return b && (i < 3 ? col === i : row === i - 3);
  });

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
  let minX = Math.min(...values);
  let maxX = Math.max(...values);
  if (minX === Infinity) minX = 0;
  if (maxX === -Infinity) maxX = 0;
  const values2 = values.map(v => minX === maxX ? 0 : (v - minX) / (maxX - minX));
  
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
    zero: minX === maxX ? 0 : -minX / (maxX - minX),
    symbols,
    graduation: range(Math.floor(minX), Math.ceil(maxX)+1)
      .map(value => ({ value, x: minX === maxX ? 0 : (value - minX) / (maxX - minX) }))
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