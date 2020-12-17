import { formatCSS, formatHTML } from "./formatting"
import { Result } from "sass"

test("formatHTML", () => {
  const html = "<div  >Hello<span >world</span></div >"
  expect(formatHTML(html)).toBe("<div>Hello<span>world</span></div>\n")
})

test("formatCSS", () => {
  const css = { css: Buffer.from("div { border  : 1px   solid blue;    }", "utf-8") } as Result
  expect(formatCSS(css).css).toBe("div {\n  border: 1px solid blue;\n}\n")
})
