import { getConfig } from "../config"
import { readdirRecursive } from "../utils"
import { buildSite, clean } from "./build"
import { brotli, compress, gzip } from "./compress"

describe("compress", () => {
  it("does not compress when developing", async () => {
    await clean()
    await buildSite(false).run()
    await compress(false)

    const files = await readdirRecursive(getConfig().out, [])
    expect(files).not.toContain("test/index.html.gz")
    expect(files).not.toContain("test/about/index.html.gz")
    expect(files).not.toContain("test/style.css.gz")
    expect(files).not.toContain("test/style.css.map.gz")
  })

  it("does compress when releasing", async () => {
    await clean()
    await buildSite(true).run()
    await compress(true)

    const files = await readdirRecursive(getConfig().out, [])
    expect(files).toContain("test/index.html.gz")
    expect(files).toContain("test/about/index.html.gz")
    expect(files).not.toContain("test/style.css.map.gz")
  })
})

test("gzip", async () => {
  await clean()
  await buildSite(false).run()
  await gzip(getConfig())

  const files = await readdirRecursive(getConfig().out, [])
  expect(files).toContain("test/index.html.gz")
  expect(files).toContain("test/about/index.html.gz")
  expect(files).toContain("test/style.css.gz")
  expect(files).not.toContain("test/style.css.map.gz")
})

test("brotli", async () => {
  await clean()
  await buildSite(false)
  await brotli(getConfig())

  const files = await readdirRecursive(getConfig().out, [])
  expect(files).toContain("test/index.html.br")
  expect(files).toContain("test/about/index.html.br")
  expect(files).toContain("test/style.css.br")
  expect(files).not.toContain("test/style.css.map.br")
})
