import path from "path"

import { defaultConfig, setConfig } from "./config"

describe("config", () => {
  it("sets correct default settings", () => {
    expect(defaultConfig.out.includes("/test")).toBeTruthy()
    expect(defaultConfig.content.pages).toBe(path.join(path.resolve(process.cwd()), "content/pages/"))
  })

  it("can be overridden", () => {
    const config = setConfig(defaultConfig, { out: "./public", production: true, url: "http://test.com" })

    expect(config.out.includes("/public")).toBeTruthy()
    expect(config.production).toBeTruthy()
    expect(config.url).toBe("http://test.com")
  })
})
