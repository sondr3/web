export type Result<T, E> = Ok<T, E> | Err<T, E>;

interface ResultImpl<T, E> {
  isOk: () => boolean;
  isErr: () => boolean;
  unwrap: () => T | E;
}

export class Ok<T, E> implements ResultImpl<T, E> {
  readonly value: T;

  constructor(value: T) {
    this.value = value;
  }

  isOk(): this is Err<T, E> {
    return true;
  }

  isErr(): this is Ok<T, E> {
    return false;
  }

  unwrap(): T {
    return this.value;
  }
}

export class Err<T, E> implements ResultImpl<T, E> {
  readonly value: E;

  constructor(value: E) {
    this.value = value;
  }

  isOk(): this is Err<T, E> {
    return false;
  }

  isErr(): this is Ok<T, E> {
    return true;
  }

  unwrap(): E {
    throw new Error("Unwrap on Err");
  }
}

export const ok = <T, E>(t: T): Result<T, E> => new Ok(t);

export const err = <T, E>(e: E): Result<T, E> => new Err(e);
