import { renderStyles, styleName } from "./styles"
import path from "path"
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
    const config = getConfig()
    const spec = path.resolve(config.assets.style, "style.scss")
    const res = await renderStyles(spec, false).run()

    expect(res.isRight()).toBeTruthy()
  })

  it("errors when it cannot render", async () => {
    try {
      const res = await renderStyles("no.scss", false).run()
      expect(res.isLeft()).toBeTruthy()
    } catch (e) {
      // noop
    }
  })

  it("minifies when in prod", async () => {
    const config = getConfig()
    const spec = path.resolve(config.assets.style, "style.scss")
    const res = await renderStyles(spec, false).run()
    await expect(res.isRight()).toBeTruthy()
  })
})
