import { AsciidocConverter } from "./AsciidocConverter";
import path from "path";

describe("AsciidocConverter", () => {
  const engine = new AsciidocConverter();

  it("renders a file", async () => {
    await expect(engine.renderAsciidoc(path.resolve(process.cwd(), "hello.adoc"))).resolves.toBeDefined();
  });
});
