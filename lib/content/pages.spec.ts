import { promises as fs } from "fs"
import path from "path"

import { TestSite } from "../tests"
import { minifyHTML, writeHTML } from "."
import { renderSpecialPages } from "./pages"

describe("renderPages", () => {
  it("renders all pages", async () => {
    const site = TestSite()
    await renderSpecialPages(site)
    expect(await fs.stat(path.join(site.config.out, "index.html"))).toBeDefined()
  })
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
