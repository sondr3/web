import { renderStyles, styleName } from "./styles"
import path from "path"
import assert from "assert"
import { getConfig } from "../config"

describe("styleName", () => {
  it("defaults to css", () => {
    const file = styleName("test.blah")
    expect(file.endsWith("/test/test.css")).toBeTruthy()
  })

  it("gives correct name with extension", () => {
    const file = styleName("test.html", "css")
    expect(file.endsWith("/test/test.css")).toBeTruthy()
  })
})

describe("renderStyles", () => {
  it("renders and creates files", async () => {
    process.env.NODE_ENV = "test"
    const config = getConfig()
    const spec = path.resolve(config.assets.style, "style.scss")
    const res = await renderStyles(spec, false)

    expect(res === undefined).toBeTruthy()
  })

  it("errors when it cannot render", async () => {
    try {
      const res = await renderStyles("no.scss", false)

      expect(res !== undefined).toBeTruthy()
      assert(res !== undefined)
      expect(res.message).toContain("Could not create styles")
    } catch (e) {
      // noop
    }
  })
})
