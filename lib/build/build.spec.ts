import { buildSite } from "./build"
import path from "path"
import { convertAsciidoc, renderAsciidoc } from "../content"

test("buildSite", async () => {
  const res = await buildSite(false)
  expect(res).toBeUndefined()
})

test("renderAsciidoc", async () => {
  const doc = await convertAsciidoc(path.resolve(process.cwd(), "content/pages/about.adoc"))
  if (doc instanceof Error) throw doc
  expect(renderAsciidoc(doc)).toBeDefined()
})
