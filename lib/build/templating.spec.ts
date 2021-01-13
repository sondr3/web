import { promises as fs } from "fs"
import path from "path"

import { renderContent } from "../content"
import { defaultConfig, Site } from "../site"
import { readFile } from "../utils"
import { Asciidoc } from "./asciidoc"
import { createTitle, renderTemplate } from "./templating"

test("renderTemplate", async () => {
  const about = await readFile(path.resolve(process.cwd(), "content/pages/about.adoc")).run()
  const document = renderContent(new Asciidoc(), about.unsafeCoerce(), { layout: "default", path: "" })
  expect(renderTemplate(new Site(), "default", document)).toBeDefined()
  await fs.rmdir(defaultConfig.out, { recursive: true })
})

test("createTitle", () => {
  expect(createTitle(defaultConfig, "EONS")).toBe(defaultConfig.meta.title)
})
