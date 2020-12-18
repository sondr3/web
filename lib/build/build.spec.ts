import { getConfig } from "../config"
import { buildSite, minifyHTML, renderAsciidoc, renderSpecialPages } from "./build"
import path from "path"
import { promises as fs } from "fs"

test("buildSite", async () => {
  const res = await buildSite(false)
  expect(res).toBeUndefined()
})

describe("renderPages", () => {
  afterAll(async () => {
    await fs.rmdir(getConfig().out)
  })

  it("renders all pages", async () => {
    const config = getConfig()
    await renderSpecialPages(false)

    expect(await fs.stat(path.join(config.out, "index.html"))).toBeDefined()
  })
})

describe("renderAsciidoc", () => {
  it("renders", async () => {
    await expect(renderAsciidoc(path.resolve(process.cwd(), "content/pages/about.adoc"), false)).resolves.toBeDefined()
  })

  it("returns unformatted when production", async () => {
    await expect(renderAsciidoc(path.resolve(process.cwd(), "content/pages/about.adoc"), true)).resolves.toBeDefined()
  })
})

test("minifyHTML", () => {
  const html = "<div  class='hello'><p class=''>Hello!</p></div>"
  expect(minifyHTML(html).toString()).toBe("<div class=hello><p>Hello!</div>")
})
