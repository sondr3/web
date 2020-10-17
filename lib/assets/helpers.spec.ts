import { copyAssets } from "./helpers"

test("copyAssets", async () => {
  expect(await copyAssets()).toBeUndefined()
})
