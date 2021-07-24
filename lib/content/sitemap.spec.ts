import { TestSite } from "../tests"
import { createDirectory } from "../utils"
import { sitemap } from "./sitemap"

test("sitemap", async () => {
  const site = TestSite()
  await createDirectory(site.config.out)
  await expect(sitemap(site)).resolves.not.toThrow()
})
