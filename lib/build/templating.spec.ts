import { promises as fs } from "fs"

import { defaultConfig, Site } from "../site"
import { createTitle, renderTemplate } from "./templating"

test("renderTemplate", async () => {
  expect(renderTemplate(new Site(), "default", { title: "", content: "hello, world!" })).toBeDefined()
  await fs.rmdir(defaultConfig.out, { recursive: true })
})

test("createTitle", () => {
  expect(createTitle(defaultConfig, "EONS")).toBe(defaultConfig.meta.title)
})
