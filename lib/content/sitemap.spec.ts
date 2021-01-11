import { defaultConfig } from "../config"
import { createDirectory } from "../utils"
import { sitemap } from "./sitemap"

test("sitemap", async () => {
  await createDirectory(defaultConfig.out)
  await expect(sitemap(defaultConfig)).resolves.not.toThrow()
})
