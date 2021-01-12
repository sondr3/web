import { defaultConfig, setConfig } from "../site"
import { copyAssets } from "./helpers"

describe("copyAssets", () => {
  it("works on default config", async () => {
    const result = await copyAssets(defaultConfig).run()
    expect(result.isRight()).toBeTruthy()
  })

  it("fails on wrong config", async () => {
    const result = await copyAssets(setConfig(defaultConfig, { out: "/no" }))
    expect(result.isLeft()).toBeTruthy()
  })
})
