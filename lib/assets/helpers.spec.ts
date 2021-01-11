import { copyAssets } from "./helpers"
import { defaultConfig } from "../config"

test("copyAssets", async () => {
  const result = await copyAssets(defaultConfig).run()
  expect(result.isRight()).toBeTruthy()
})
