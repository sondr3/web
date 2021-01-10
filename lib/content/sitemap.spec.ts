import { getConfig } from "../config"
import { createDirectory } from "../utils"
import { sitemap } from "./sitemap"

const config = getConfig()

test("sitemap", async () => {
  await createDirectory(config.out)
  await expect(sitemap()).resolves.not.toThrow()
})
