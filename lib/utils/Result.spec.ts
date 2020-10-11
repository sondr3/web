import { err, ok } from "./Result";

describe("Result", () => {
  describe("Ok", () => {
    it("constructs", () => {
      const res = ok("Hello");

      expect(res.isOk()).toBeTruthy();
      expect(res.unwrap()).toBe("Hello");
      expect(res.isErr()).toBeFalsy();
    });
  });

  describe("Err", () => {
    it("constructs", () => {
      const res = err("Hello");

      expect(res.isErr()).toBeTruthy();
      expect(res.unwrap.bind(res)).toThrow("Unwrap on Err");
      expect(res.isOk()).toBeFalsy();
    });
  });
});
