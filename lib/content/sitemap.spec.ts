import { Site } from "../site"
import { createDirectory } from "../utils"
import { sitemap } from "./sitemap"

test("sitemap", async () => {
  await createDirectory(new Site().config.out)
  await expect(sitemap(new Site())).resolves.not.toThrow()
})
