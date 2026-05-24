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


type PathValue<T, P extends readonly (string | number)[]> =
  P extends readonly []
  ? T
  : P extends readonly [infer K, ...infer Rest extends (string | number)[]]
  ? K extends keyof T ? PathValue<T[K], Rest> : never
  : never;

export function update<T, const P extends readonly (string | number)[]>(
  obj: T,
  path: P,
  updater: (x: PathValue<T, P>) => PathValue<T, P>
): T {
  const result: T = Array.isArray(obj) ? [...(obj as any)] : { ...(obj as any) };
  let current: any = result;

  for (let i = 0; i < path.length - 1; i++) {
    const key = path[i];
    current[key] = Array.isArray(current[key]) ? [...current[key]] : { ...current[key] };
    current = current[key];
  }

  const lastKey = path[path.length - 1];
  current[lastKey] = updater(current[lastKey]);

  return result;
}