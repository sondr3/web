import { promises as fs } from "fs"
import { getConfig } from "./config"
import { createTitle, renderTemplate } from "./templating"

describe("renderTemplate", () => {
  afterAll(async () => {
    await fs.rmdir(getConfig().out)
  })

  it("renders default layout", () => {
    expect(renderTemplate("default", { title: "", content: "hello, world!" })).toBeDefined()
  })
})

describe("createTitle", () => {
  it("doesn't duplicate title", () => {
    const config = getConfig()
    expect(createTitle("EONS")).toBe(config.meta.title)
  })
})
