export function scalarProduct(xs: number[], ys: number[]) {
  let res = 0;
  const n = xs.length;
  for (let i = 0; i < n; i++) {
    res += xs[i] * ys[i];
  }
  return res;
}