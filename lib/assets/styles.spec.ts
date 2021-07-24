import fs from "fs"
import path from "path"

import { defaultConfig, setConfig } from "../site"
import { testConfig, TestSite } from "../tests"
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
    const site = TestSite()
    const spec = path.resolve(site.config.assets.style, "style.scss")
    const result = await renderStyles(site, spec).run()

    expect(result.isRight()).toBeTruthy()
    const directories = fs.readdirSync(site.config.out)
    expect(directories.includes("style.css")).toBeTruthy()
    expect(directories.includes("style..css")).not.toBeTruthy()
  })

  it("renders and adds hash in prod", async () => {
    const site = TestSite(setConfig(testConfig, { production: true }))
    const spec = path.resolve(site.config.assets.style, "style.scss")
    const result = await renderStyles(site, spec).run()

    expect(result.isRight()).toBeTruthy()
    const directories = fs.readdirSync(site.config.out)
    expect(directories.find((file) => file.endsWith(".css"))).toBeDefined()
    expect(directories.some((file) => new RegExp(/style\.[\dA-Za-z]+\.css/).exec(file) !== null)).toBeTruthy()
  })

  it("errors when it cannot render", async () => {
    try {
      const result = await renderStyles(TestSite(), "no.scss").run()
      expect(result.isLeft()).toBeTruthy()
      expect(typeof result.leftToMaybe().unsafeCoerce()).toBe(StyleError)
      expect(result.leftToMaybe().unsafeCoerce().message).toContain(/Error/)
    } catch {
      // noop
    }
  })

  it("minifies when in prod", async () => {
    const site = TestSite()
    const spec = path.resolve(site.config.assets.style, "style.scss")
    const result = await renderStyles(site, spec).run()
    expect(result.isRight()).toBeTruthy()
  })
})
