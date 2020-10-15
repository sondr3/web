import { formatCSS, formatHTML } from "./formatting"

describe("formatHTML", () => {
  it("formats HTML", () => {
    const html = "<div  >Hello<span >world</span></div >"
    expect(formatHTML(html)).toBe("<div>Hello<span>world</span></div>\n")
  })
})

describe("formatCSS", () => {
  it("formats css", () => {
    const css = "div { border  : 1px   solid blue;    }"
    expect(formatCSS(css)).toBe("div {\n  border: 1px solid blue;\n}\n")
  })
})
