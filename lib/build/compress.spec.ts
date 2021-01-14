import { defaultConfig, Site } from "../site"
import { readdirRecursive } from "../utils"
import { buildSite, clean } from "./build"
import { brotli, compress, gzip } from "./compress"

describe("compress", () => {
  it("does not compress when developing", async () => {
    await clean(defaultConfig)
    await buildSite(new Site()).run()
    await compress(defaultConfig)

    const files = await readdirRecursive(defaultConfig.out, [])
    expect(files).not.toContain("test/index.html.gz")
    expect(files).not.toContain("test/about/index.html.gz")
    expect(files).not.toContain("test/style.css.gz")
    expect(files).not.toContain("test/style.css.map.gz")
  })

  it("does compress when releasing", async () => {
    await clean(defaultConfig)
    await buildSite(new Site()).run()
    await compress(defaultConfig)

    const files = await readdirRecursive(defaultConfig.out, [])
    expect(files).toContain("test/index.html.gz")
    expect(files).toContain("test/about/index.html.gz")
    expect(files).not.toContain("test/style.css.map.gz")
  })
})

test("gzip", async () => {
  await clean(defaultConfig)
  await buildSite(new Site()).run()
  await gzip(defaultConfig)

  const files = await readdirRecursive(defaultConfig.out, [])
  expect(files).toContain("test/index.html.gz")
  expect(files).toContain("test/about/index.html.gz")
  expect(files).toContain("test/style.css.gz")
  expect(files).not.toContain("test/style.css.map.gz")
})

test("brotli", async () => {
  await clean(defaultConfig)
  await buildSite(new Site())
  await brotli(defaultConfig)

  const files = await readdirRecursive(defaultConfig.out, [])
  expect(files).toContain("test/index.html.br")
  expect(files).toContain("test/about/index.html.br")
  expect(files).toContain("test/style.css.br")
  expect(files).not.toContain("test/style.css.map.br")
})
