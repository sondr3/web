import { defaultConfig } from "../config"
import { copyAssets } from "./helpers"

test("copyAssets", async () => {
  const result = await copyAssets(defaultConfig).run()
  expect(result.isRight()).toBeTruthy()
})
