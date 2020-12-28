import { promises as fs } from "fs"
import { getConfig } from "../config"
import { renderSpecialPages } from "./pages"
import path from "path"
import { convertAsciidoc, minifyHTML, renderAsciidoc, writeHTML } from "./index"

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

test("convertAsciidoc", async () => {
  await expect(convertAsciidoc(path.resolve(process.cwd(), "content/pages/about.adoc"))).resolves.toBeDefined()
})

test("renderAsciidoc", async () => {
  console.error(path.resolve(process.cwd(), "content/pages/about.adoc"))
  const doc = await convertAsciidoc(path.resolve(process.cwd(), "content/pages/about.adoc"))
  if (doc instanceof Error) throw doc
  await expect(renderAsciidoc(doc)).resolves.toBeDefined()
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
