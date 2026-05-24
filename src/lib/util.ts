export function scalarProduct(xs: readonly number[], ys: readonly number[]) {
  let res = 0;
  const n = xs.length;
  for (let i = 0; i < n; i++) {
    res += xs[i] * ys[i];
  }
  return res;
}