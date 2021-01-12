import { promises as fs } from "fs"
import path from "path"

import { defaultConfig, Site } from "../site"
import { convertAsciidoc, minifyHTML, writeHTML } from "."
import { renderSpecialPages } from "./pages"

describe("renderPages", () => {
  afterAll(async () => {
    await fs.rmdir(defaultConfig.out)
  })

  it("renders all pages", async () => {
    await renderSpecialPages(new Site(), false)

    expect(await fs.stat(path.join(defaultConfig.out, "index.html"))).toBeDefined()
  })
})

test("convertAsciidoc", async () => {
  await expect(convertAsciidoc(path.resolve(process.cwd(), "content/pages/about.adoc"))).resolves.toBeDefined()
})

test("minifyHTML", () => {
  const html = "<div  class='hello'><p class=''>Hello!</p></div>"
  expect(minifyHTML(html).toString()).toBe("<div class=hello><p>Hello!</div>")
})

describe("writeHTML", () => {
  it("formats HTML in dev mode", () => {
    expect(writeHTML(`<div   class="hello">\n<p class=""   >Hello</p></div>`, false)).toBe(
      `<div class="hello">\n  <p class="">Hello</p>\n</div>\n`,
    )
  })

  it("minifies HTML in dev mode", () => {
    expect(writeHTML(`<div   class="hello">\n<p class=""   >Hello</p></div>`, true).toString()).toBe(
      "<div class=hello><p>Hello</div>",
    )
  })
})
