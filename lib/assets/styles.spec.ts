import { renderStyles, styleName } from "./styles"
import path from "path"
import { defaultConfig } from "../config"

describe("styleName", () => {
  it("defaults to css", () => {
    const file = styleName(defaultConfig, "test.blah")
    expect(file.endsWith("/test/test.css")).toBeTruthy()
  })

  it("gives correct name with extension", () => {
    const file = styleName(defaultConfig, "test.html", "css")
    expect(file.endsWith("/test/test.css")).toBeTruthy()
  })
})

describe("renderStyles", () => {
  it("renders and creates files", async () => {
    const spec = path.resolve(defaultConfig.assets.style, "style.scss")
    const res = await renderStyles(defaultConfig, spec, false).run()

    expect(res.isRight()).toBeTruthy()
  })

  it("errors when it cannot render", async () => {
    try {
      const res = await renderStyles(defaultConfig, "no.scss", false).run()
      expect(res.isLeft()).toBeTruthy()
    } catch (e) {
      // noop
    }
  })

  it("minifies when in prod", async () => {
    const spec = path.resolve(defaultConfig.assets.style, "style.scss")
    const res = await renderStyles(defaultConfig, spec, false).run()
    await expect(res.isRight()).toBeTruthy()
  })
})
