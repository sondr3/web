import { copyAssets } from "./helpers"

describe("copyAssets", () => {
  it("works", async () => {
    expect(await copyAssets()).toBeUndefined()
  })
})
