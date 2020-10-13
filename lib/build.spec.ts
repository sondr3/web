import { getConfig } from "./config";
import { buildSite, renderAsciidoc, renderPages } from "./build";
import path from "path";
import { promises as fs } from "fs";

describe("buildSite", () => {
  it("builds", async () => {
    process.env.NODE_ENV = "test";
    const res = await buildSite();
    expect(res).toBeUndefined();
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

describe("renderAsciidoc", () => {
  it("renders", async () => {
    await expect(renderAsciidoc(path.resolve(process.cwd(), "content/pages/about.adoc"))).resolves.toBeDefined();
  });
});
