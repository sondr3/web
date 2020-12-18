import { getConfig } from "../config"
import { readdirRecursive } from "../utils"
import { buildSite } from "./build"
import { brotli, gzip } from "./compress"

test("gzip", async () => {
  await buildSite(false)
  await gzip(getConfig())

  const files = await readdirRecursive(getConfig().out, [])
  expect(files).toContain("test/index.html.gz")
  expect(files).toContain("test/about/index.html.gz")
  expect(files).toContain("test/style.css.gz")
  expect(files).not.toContain("test/style.css.map.gz")
})

test("brotli", async () => {
  await buildSite(false)
  await brotli(getConfig())

  const files = await readdirRecursive(getConfig().out, [])
  expect(files).toContain("test/index.html.br")
  expect(files).toContain("test/about/index.html.br")
  expect(files).toContain("test/style.css.br")
  expect(files).not.toContain("test/style.css.map.br")
})
