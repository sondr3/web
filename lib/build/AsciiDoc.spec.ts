import path from "path"

import { Asciidoc } from "./Asciidoc"

test("Asciidoc", async () => {
  const engine = new Asciidoc()

  await expect(engine.load(path.resolve(process.cwd(), "content/pages/about.adoc"))).resolves.toBeDefined()
})
