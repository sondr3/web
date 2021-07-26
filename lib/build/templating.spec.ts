import path from "path"

import { renderContent } from "../content"
import { defaultConfig } from "../site"
import { TestSite } from "../tests"
import { readFile } from "../utils"
import { Asciidoc } from "./asciidoc"
import { createTitle, renderTemplate } from "./templating"

test("renderTemplate", async () => {
  const about = await readFile(path.resolve(process.cwd(), "content/pages/about.adoc")).run()
  const document = renderContent(new Asciidoc(), about.unsafeCoerce(), { layout: "default", path: "" })
  expect(renderTemplate(TestSite(), "default", document)).toBeDefined()
})

test("createTitle", () => {
  expect(createTitle(defaultConfig, "EONS")).toBe(defaultConfig.meta.title)
})
