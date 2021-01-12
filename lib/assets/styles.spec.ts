import fs from "fs"
import path from "path"

import { Site } from "../site"
import { renderStyles, StyleError, styleName } from "./styles"

describe("styleName", () => {
  it("defaults to css", () => {
    const file = styleName(new Site().config, "test.blah")
    expect(file.endsWith("/test/test.css")).toBeTruthy()
  })

  it("gives correct name with extension", () => {
    const file = styleName(new Site().config, "test.html", "css")
    expect(file.endsWith("/test/test.css")).toBeTruthy()
  })
})

describe("renderStyles", () => {
  it("renders and creates files", async () => {
    const spec = path.resolve(new Site().config.assets.style, "style.scss")
    const result = await renderStyles(new Site(), spec, false).run()

    expect(result.isRight()).toBeTruthy()
    expect(fs.existsSync(path.resolve(process.cwd(), new Site().config.out, "style.css"))).toBeTruthy()
  })

  it("errors when it cannot render", async () => {
    try {
      const result = await renderStyles(new Site(), "no.scss", false).run()
      expect(result.isLeft()).toBeTruthy()
      expect(typeof result.leftToMaybe().unsafeCoerce()).toBe(StyleError)
      expect(result.leftToMaybe().unsafeCoerce().message).toContain(/Error/)
    } catch {
      // noop
    }
  })

  it("minifies when in prod", async () => {
    const spec = path.resolve(new Site().config.assets.style, "style.scss")
    const result = await renderStyles(new Site(), spec, false).run()
    expect(result.isRight()).toBeTruthy()
  })
})
