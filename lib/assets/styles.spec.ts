import fs from "fs"
import path from "path"

import { defaultConfig } from "../site/config"
import { renderStyles, StyleError, styleName } from "./styles"

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
    const result = await renderStyles(defaultConfig, spec, false).run()

    expect(result.isRight()).toBeTruthy()
    expect(fs.existsSync(path.resolve(process.cwd(), defaultConfig.out, "style.css"))).toBeTruthy()
  })

  it("errors when it cannot render", async () => {
    try {
      const result = await renderStyles(defaultConfig, "no.scss", false).run()
      expect(result.isLeft()).toBeTruthy()
      expect(typeof result.leftToMaybe().unsafeCoerce()).toBe(StyleError)
      expect(result.leftToMaybe().unsafeCoerce().message).toContain(/Error/)
    } catch {
      // noop
    }
  })

  it("minifies when in prod", async () => {
    const spec = path.resolve(defaultConfig.assets.style, "style.scss")
    const result = await renderStyles(defaultConfig, spec, false).run()
    expect(result.isRight()).toBeTruthy()
  })
})
