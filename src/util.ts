export function replicate<A>(n: number, val: A) {
  let a = new Array(n);
  a.fill(val);
  return a;
}

export function scalarProduct(xs: number[], ys: number[]) {
  let res = 0;
  const n = xs.length;
  for (let i = 0; i < n; i++) {
    res += xs[i] * ys[i];
  }
  return res;
}

export function zipWith<A, B, C>(xs: A[], ys: B[], fn: (x: A, y: B) => C): C[] {
  return xs.map((x, i) => fn(x, ys[i]))
}