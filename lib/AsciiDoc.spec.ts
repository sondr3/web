import { Asciidoc } from "./Asciidoc";
import path from "path";

describe("Asciidoc", () => {
  const engine = new Asciidoc();

  it("renders a file", async () => {
    await expect(engine.render(path.resolve(process.cwd(), "content/pages/about.adoc"))).resolves.toBeDefined();
  });
});
