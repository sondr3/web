import { setConfig } from "../site"
import { testConfig } from "../tests"
import { copyAssets } from "./helpers"

describe("copyAssets", () => {
  it("works on default config", async () => {
    const result = await copyAssets(testConfig).run()
    expect(result.isRight()).toBeTruthy()
  })

  it("fails on wrong config", async () => {
    const result = await copyAssets(setConfig(testConfig, { out: "/no" }))
    expect(result.isLeft()).toBeTruthy()
  })
})
