import { sitemap } from "./sitemap"
import { createDirectory } from "../utils"
import { getConfig } from "../config"

const config = getConfig()

test("sitemap", async () => {
  await createDirectory(config.out)
  await expect(sitemap()).resolves.not.toThrow()
})
