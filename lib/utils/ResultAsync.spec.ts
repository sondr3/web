import { errAsync, okAsync, ResultAsync } from "./ResultAsync";
import { Err, Ok } from "./Result";

describe("ResultAsync", () => {
  it("ok", async () => {
    const res = okAsync(42);
    const actual = await res;
    expect(actual).toBeInstanceOf(Ok);
    expect(actual.unwrap()).toBe(42);
  });

  it("err", async () => {
    const res = errAsync("No");
    const actual = await res;
    expect(actual).toBeInstanceOf(Err);
    expect(actual.unwrap.bind(actual)).toThrow("Unwrap on Err");
  });

  it("fromPromise without error handler", async () => {
    const res = ResultAsync.fromPromise(Promise.resolve(42));
    const actual = await res;
    expect(actual.isOk()).toBeTruthy();
    expect(actual.unwrap()).toBe(42);
  });

  it("fromPromise with error handler", async () => {
    const res = ResultAsync.fromPromise(Promise.reject(42), () => new Error("No"));
    const actual = await res;
    expect(actual.isErr).toBeTruthy();
    expect(actual.unwrap.bind(actual)).toThrow("Unwrap on Err");
  });
});
