import { getConfig, setConfig } from "./config"
import path from "path"

describe("config", () => {
  it("sets correct default settings", () => {
    const config = getConfig()

    expect(config.out.includes("/public")).toBeTruthy()
    expect(config.content.pages).toBe(path.join(path.resolve(process.cwd()), "content/pages/"))
  })

  it("can be overridden", () => {
    setConfig(true, "./public", "http://test.com")
    const config = getConfig()

    expect(config.out.includes("/public")).toBeTruthy()
    expect(config.production).toBeTruthy()
    expect(config.meta.url).toBe("http://test.com")
  })
})
