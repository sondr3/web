import { Err, Ok, Result } from "./Result";
import { logging } from "./logging";

const logger = logging.getLogger("ResultAsync");

export class ResultAsync<T, E> implements PromiseLike<Result<T, E>> {
  private readonly promise: Promise<Result<T, E>>;

  constructor(res: Promise<Result<T, E>>) {
    this.promise = res;
  }

  static fromPromise<T, E>(promise: Promise<T>, error?: (e: unknown) => E): ResultAsync<T, E> {
    let res: Promise<Result<T, E>> = promise.then((val: T) => new Ok(val));
    if (error) {
      res = res.catch((e) => new Err<T, E>(error(e)));
    } else {
      logger.warn("fromPromise called without error handling");
    }

    return new ResultAsync<T, E>(res);
  }

  then<A, B>(
    ok?: (res: Result<T, E>) => A | PromiseLike<A>,
    err?: (e: unknown) => B | PromiseLike<B>,
  ): PromiseLike<A | B> {
    return this.promise.then(ok, err);
  }
}

export const okAsync = <T, E>(t: T): ResultAsync<T, E> => new ResultAsync(Promise.resolve(new Ok(t)));

export const errAsync = <T, E>(e: E): ResultAsync<T, E> => new ResultAsync(Promise.resolve(new Err(e)));
