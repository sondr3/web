import path from "path"

import { defaultConfig, getConfig, setConfig } from "./config"

describe("config", () => {
  it("sets correct default settings", () => {
    const config = getConfig()

    expect(config.out.includes("/test")).toBeTruthy()
    expect(config.content.pages).toBe(path.join(path.resolve(process.cwd()), "content/pages/"))
  })

  it("can be overridden", () => {
    const config = setConfig(defaultConfig, { out: "./public", production: true, meta: { url: "http://test.com" } })

    expect(config.out.includes("/public")).toBeTruthy()
    expect(config.production).toBeTruthy()
    expect(config.meta.url).toBe("http://test.com")
  })
})
