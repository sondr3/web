import { getConfig, setConfig } from "./config"
import path from "path"

describe("config", () => {
  describe("test", () => {
    it("gets the correct configuration", () => {
      const config = getConfig()

      expect(config.out.includes("/test")).toBeTruthy()
      expect(config.content.pages).toBe(path.join(path.resolve(process.cwd()), "content/pages/"))
    })
  })

  describe("production", () => {
    beforeEach(() => setConfig(true, "./public", "http://test.com"))

    it("gets the correct configuration", () => {
      const config = getConfig()

      expect(config.out.includes("/public")).toBeTruthy()
      expect(config.production).toBeTruthy()
      expect(config.meta.url).toBe("http://test.com")
    })
  })
})
