import { Asciidoc } from "./Asciidoc"
import path from "path"

test("Asciidoc", async () => {
  const engine = new Asciidoc()

  await expect(engine.load(path.resolve(process.cwd(), "content/pages/about.adoc"))).resolves.toBeDefined()
})
