import { getConfig } from "./config";
import { renderPage, renderPages } from "./build";
import path from "path";
import { promises as fs } from "fs";

describe("renderPage", () => {
  beforeEach(() => {
    process.env.NODE_ENV = "test";
  });

  afterAll(async () => {
    await fs.rmdir(getConfig().out);
  });

  it("can render a page", async () => {
    const config = getConfig();

    await renderPage(path.join(config.content.pages, "index.liquid"), config.out);
    expect(await fs.stat(path.join(config.out, "index.html"))).toBeDefined();
  });

  it("cannot render a missing template", async () => {
    const config = getConfig();

    await expect(renderPage(path.join(config.content.pages, "wrong.liquid"), config.out)).rejects.toThrow();
  });
});

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
