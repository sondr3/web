import { promises as fs } from "fs"

import { defaultConfig } from "../site/config"
import { createTitle, renderTemplate } from "./templating"

test("renderTemplate", async () => {
  expect(renderTemplate(defaultConfig, "default", { title: "", content: "hello, world!" })).toBeDefined()
  await fs.rmdir(defaultConfig.out, { recursive: true })
})

test("createTitle", () => {
  expect(createTitle(defaultConfig, "EONS")).toBe(defaultConfig.meta.title)
})
