import { promises as fs } from "fs";
import { getConfig } from "./config";
import path from "path";
import { TemplateEngine } from "./template";

describe("template", () => {
  const engine = new TemplateEngine();

  beforeEach(() => {
    process.env.NODE_ENV = "test";
  });

  afterAll(async () => {
    await fs.rmdir(getConfig().out);
  });

  it("can render a page", async () => {
    const config = getConfig();

    await engine.render(path.join(config.content.pages, "index.liquid"), config.out);
    expect(await fs.stat(path.join(config.out, "index.html"))).toBeDefined();
  });

  it("cannot render a missing template", async () => {
    const config = getConfig();

    await expect(engine.render(path.join(config.content.pages, "wrong.liquid"), config.out)).rejects.toThrow();
  });
});
