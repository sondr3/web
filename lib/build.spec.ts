import { getConfig } from "./config";
import { buildSite, renderAsciidoc, renderSpecialPages } from "./build";
import path from "path";
import { promises as fs } from "fs";

describe("buildSite", () => {
  it("builds", async () => {
    process.env.NODE_ENV = "test";
    const res = await buildSite(false);
    expect(res).toBeUndefined();
  });
});

describe("renderPages", () => {
  afterAll(async () => {
    await fs.rmdir(getConfig().out);
  });

  it("renders all pages", async () => {
    process.env.NODE_ENV = "test";
    const config = getConfig();
    await renderSpecialPages();

    expect(await fs.stat(path.join(config.out, "index.html"))).toBeDefined();
  });
});

describe("renderAsciidoc", () => {
  it("renders", async () => {
    await expect(renderAsciidoc(path.resolve(process.cwd(), "content/pages/about.adoc"), false)).resolves.toBeDefined();
  });

  it("returns unformatted when production", async () => {
    await expect(renderAsciidoc(path.resolve(process.cwd(), "content/pages/about.adoc"), true)).resolves.toBeDefined();
  });
});
