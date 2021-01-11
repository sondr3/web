import path from "path"

import { defaultConfig } from "../config"
import { convertAsciidoc, renderAsciidoc } from "../content"
import { buildSite } from "./build"

test("buildSite", async () => {
  const result = await buildSite(defaultConfig, false).run()
  expect(result.isRight()).toBeTruthy()
})

test("renderAsciidoc", async () => {
  const document = await convertAsciidoc(path.resolve(process.cwd(), "content/pages/about.adoc")).run()
  expect(document.isRight()).toBeTruthy()
  expect(renderAsciidoc(defaultConfig, document.unsafeCoerce())).toBeDefined()
})
