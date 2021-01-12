import path from "path"

import { convertAsciidoc, renderAsciidoc } from "../content"
import { defaultConfig } from "../site/config"
import { buildSite } from "./build"

describe("buildSite", () => {
  it("works with default config", async () => {
    const result = await buildSite(defaultConfig, false).run()
    expect(result.isRight()).toBeTruthy()
  })
})

test("renderAsciidoc", async () => {
  const document = await convertAsciidoc(path.resolve(process.cwd(), "content/pages/about.adoc")).run()
  expect(document.isRight()).toBeTruthy()
  expect(renderAsciidoc(defaultConfig, document.unsafeCoerce())).toBeDefined()
})
