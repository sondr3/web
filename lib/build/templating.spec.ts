import { promises as fs } from "fs"
import { getConfig } from "../config"
import { createTitle, renderTemplate } from "./templating"

test("renderTemplate", async () => {
  expect(renderTemplate("default", { title: "", content: "hello, world!" })).toBeDefined()
  await fs.rmdir(getConfig().out, { recursive: true })
})

test("createTitle", () => {
  const config = getConfig()
  expect(createTitle("EONS")).toBe(config.meta.title)
})
