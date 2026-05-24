import { MASK, type Pattern, type State, updateOutput } from "./model";

const STEP = 0.0001; // le pas dans le flot gradient
const COEFF = 0.7;    // coef dans la fonction de coût d'erreur qui est une fonction exponentielle
const NB_ITERS = 60_000;
const ITER_LIST = [0, 100, 200, 300, 400, 500, 1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000, 40000, 50000];

const dercost = (x: number, b: boolean) => b ? -COEFF * Math.exp(-COEFF * x) : COEFF * Math.exp(COEFF * x);

export function learnOneStep(st: State, patterns: readonly Pattern[], inputs: readonly number[][], mask: readonly boolean[][]): State {
  const { iter, hiddenThresholds, hiddenWeights, finalThresholds, finalWeights, output } = st;
  const finalThresholds2 = [...finalThresholds];
  const finalWeights2 = finalWeights.map(x => [...x]);
  const hiddenThresholds2 = [...hiddenThresholds];
  const hiddenWeights2 = hiddenWeights.map(x => [...x]);

  for (let n = 0; n < 24; n++) {
    const { selected, symbol } = patterns[n];
    if (!selected) continue;
    const { final, hidden } = output[n];
    const input = inputs[n];

    for (let i = 0; i < 4; i++) {
        finalThresholds2[i] += 10 * STEP * dercost(final[i], i === symbol)
    }

    for (let i = 0; i < 4; i++) {
      for (let j = 0; j < 6; j++) {
        finalWeights2[i][j] -= STEP * dercost(final[i], symbol === i) * hidden[j]
      }
    }

    for (let k = 0; k < 6; k++) {
      if (hidden[k] > 0) {
        for (let j = 0; j < 4; j++) {
          hiddenThresholds2[k] += STEP * dercost(final[j], symbol === j) * hiddenWeights[j][k];
        }
      }
    }

    for (let k = 0; k < 6; k++) {
      if (hidden[k] > 0) {
        for (let i = 0; i < 6; i++) {
          if (mask[k][i]) {
            for (let j = 0; j < 4; j++) {
              hiddenWeights2[k][i] -= STEP * dercost(final[j], symbol === j) * finalWeights[j][k] * input[i];
            }
          }
        }
      }
    }
  }

  return {
    iter: iter + 1,
    finalThresholds: finalThresholds2,
    finalWeights: finalWeights2,
    hiddenThresholds: hiddenThresholds2,
    hiddenWeights: hiddenWeights2,
    output: [],
  }
}

export function runLearning(inputs: number[][], state: State, patterns: readonly Pattern[]): State[] {
  const res = [];
  let st = state;
  for (let i = 0; i < NB_ITERS; i++) {
    if (ITER_LIST.includes(i)) {
      res.push(st);
    }
    st = learnOneStep(st, patterns, inputs, MASK)
    st = {...st, output: updateOutput(inputs, st)};
  }
  res.push(st);
  return res
}