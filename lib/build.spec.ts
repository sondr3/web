import { getConfig } from "./config";
import { renderPages } from "./build";
import path from "path";
import { promises as fs } from "fs";

describe("renderPages", () => {
  beforeEach(() => {
    process.env.NODE_ENV = "test";
  });

  afterAll(async () => {
    await fs.rmdir(getConfig().out);
  });

  it("renders all pages", async () => {
    process.env.NODE_ENV = "test";
    const config = getConfig();
    await renderPages();

    expect(await fs.stat(path.join(config.out, "index.html"))).toBeDefined();
  });
});
