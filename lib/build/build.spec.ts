import path from "path"

import { convertAsciidoc, renderAsciidoc } from "../content"
import { buildSite } from "./build"

test("buildSite", async () => {
  const res = await buildSite(false)
  expect(res).toBeUndefined()
})

test("renderAsciidoc", async () => {
  const document = await convertAsciidoc(path.resolve(process.cwd(), "content/pages/about.adoc"))
  if (document instanceof Error) throw document
  expect(renderAsciidoc(document)).toBeDefined()
})
