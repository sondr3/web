import { getConfig } from "./config";

describe("config", () => {
  describe("test", () => {
    beforeEach(() => (process.env.NODE_ENV = "test"));

    it("gets the correct configuration", () => {
      const config = getConfig();

      expect(config.out).toBe("./dist/test/");
    });
  });

  describe("dev", () => {
    beforeEach(() => (process.env.NODE_ENV = "development"));

    it("gets the correct configuration", () => {
      const config = getConfig();

      expect(config.out).toBe("./dist/");
    });
  });

  describe("prod", () => {
    beforeEach(() => (process.env.NODE_ENV = "production"));

    it("gets the correct configuration", () => {
      const config = getConfig();

      expect(config.out).toBe("./dist/");
    });
  });
});
