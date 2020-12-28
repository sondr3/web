import { sitemap } from "./sitemap"

test("sitemap", async () => {
  await expect(sitemap()).resolves.not.toThrow()
})
