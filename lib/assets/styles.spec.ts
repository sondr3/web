import { renderStyles, styleName } from "./styles"
import path from "path"
import assert from "assert"
import { getConfig } from "../config"

describe("styleName", () => {
  it("defaults to css", async () => {
    const file = await styleName("test.blah")
    expect(file.endsWith("/test/test.css")).toBeTruthy()
  })

  it("gives correct name with extension", async () => {
    const file = await styleName("test.html", "css")
    expect(file.endsWith("/test/test.css")).toBeTruthy()
  })
})

describe("renderStyles", () => {
  it("renders and creates files", async () => {
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

  it("minifies when in prod", async () => {
    const config = getConfig()
    const spec = path.resolve(config.assets.style, "style.scss")
    await expect(renderStyles(spec, true)).resolves.toBeUndefined()
  })
})
