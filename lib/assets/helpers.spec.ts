import { copyAssets } from "./helpers"

test("copyAssets", async () => {
  const result = await copyAssets().run()
  expect(result.isRight()).toBeTruthy()
})
