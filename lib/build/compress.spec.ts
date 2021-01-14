import path from "path"

import { renderStyles } from "../assets"
import { buildPages } from "../content"
import { setConfig } from "../site"
import { testConfig, TestSite } from "../tests"
import { readdirRecursive } from "../utils"
import { brotli, compress, gzip } from "./compress"

describe("compress", () => {
  it("does not compress when developing", async () => {
    const site = TestSite()
    await buildPages(site).run()
    await renderStyles(site, path.join(site.config.assets.style, "style.scss")).run()
    await compress(site.config)

    const files = await readdirRecursive(site.config.out, [])
    expect(files).not.toContain("test/index.html.gz")
    expect(files).not.toContain("test/about/index.html.gz")
    expect(files).not.toContain("test/style.css.gz")
    expect(files).not.toContain("test/style.css.map.gz")
  })

  it("does compress when releasing", async () => {
    const site = TestSite(setConfig(testConfig, { production: true }))
    await buildPages(site).run()
    await renderStyles(site, path.join(site.config.assets.style, "style.scss")).run()
    await compress(site.config)

    const files = await readdirRecursive(site.config.out, [])
    expect(files).toContain(`${site.config.out}/index.html.gz`)
    expect(files).toContain(`${site.config.out}/about/index.html.gz`)
    expect(files).not.toContain(`${site.config.out}/style.css.map.gz`)
  })
})

test("gzip", async () => {
  const site = TestSite(setConfig(testConfig, { production: false }))
  await buildPages(site).run()
  await renderStyles(site, path.join(site.config.assets.style, "style.scss")).run()
  await gzip(site.config)

  const files = await readdirRecursive(site.config.out, [])
  expect(files).toContain(`${site.config.out}/index.html.gz`)
  expect(files).toContain(`${site.config.out}/about/index.html.gz`)
  expect(files).toContain(`${site.config.out}/style.css.gz`)
  expect(files.some((file) => new RegExp(/style\.[\dA-Za-z]+\.css/).exec(file))).toBeTruthy()
  expect(files).not.toContain(`${site.config.out}/style.css.map.gz`)
})

test("brotli", async () => {
  const site = TestSite(setConfig(testConfig, { production: true }))
  await renderStyles(site, path.join(site.config.assets.style, "style.scss")).run()
  await brotli(site.config)

  const files = await readdirRecursive(site.config.out, [])
  expect(files).toContain(`${site.config.out}/index.html.br`)
  expect(files).toContain(`${site.config.out}/about/index.html.br`)
  expect(files).toContain(`${site.config.out}/style.css.br`)
  expect(files.some((file) => new RegExp(/style\.[\dA-Za-z]+\.css/).exec(file))).toBeTruthy()
  expect(files).not.toContain(`${site.config.out}/style.css.map.br`)
})
