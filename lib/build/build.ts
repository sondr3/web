import { createDirectory, dirWalk, Duration, formatHTML, writeFile } from "../utils/"
import { getConfig } from "../config"
import path from "path"
import { logging } from "../logging"
import { Asciidoc, compress, Layout, renderTemplate } from "./"
import { copyAssets, renderStyles } from "../assets"
import * as pages from "../templates/pages"
import { createConfiguration, minify } from "@minify-html/js"
import { promises as fs } from "fs"

const logger = logging.getLogger("build")
const config = getConfig()

const asciidoc = new Asciidoc()

/**
 * Build the whole site by copying assets, building styles and all pages, posts etc.
 *
 * @param prod - Whether to optimize output
 */
export const buildSite = async (prod: boolean): Promise<void> => {
  logger.log(`Building site ${config.meta.title} (${config.meta.url})`)
  const duration = new Duration()
  await copyAssets()
  await renderStyles(path.join(getConfig().assets.style, "style.scss"), prod)
  await renderSpecialPages(prod)
  await renderPages(prod)
  await createRootFiles()
  await compress(prod)
  duration.end()
  logger.log(`Took ${duration.result()} to build site`)
}

/**
 * Render all pages in the `pages` directory in {@link config}.
 *
 * @param prod - Whether to optimize output
 */
export const renderPages = async (prod: boolean): Promise<void | Error> => {
  const pages = await dirWalk(path.resolve(process.cwd(), config.content.pages), "adoc", false)

  for (const page of pages) {
    const rendered = await renderAsciidoc(page)
    if (rendered instanceof Error) return rendered
    const file = path.parse(page)
    await writeContent(path.resolve(config.out, file.name), writeHTML(rendered, prod))
  }
}

/**
 * Renders "special" pages, e.g. landing page, 404 and such.
 *
 * @param prod - Whether to optimize output
 */
export const renderSpecialPages = async (prod: boolean): Promise<void> => {
  await writeContent(config.out, writeHTML(pages.landing(), prod))
  await writeFile(path.join(config.out, "404.html"), writeHTML(pages.notFound(), prod))
  await writeContent(path.resolve(config.out, "404/"), writeHTML(pages.notFound(), prod))
}

/**
 * Create assorted files that are often found in the root of webpages, e.g.
 * `robots.txt` and so on.
 */
export const createRootFiles = async (): Promise<void> => {
  await writeFile(path.join(config.out, "CNAME"), "www.eons.io")
}

/**
 * Renders a Asciidoctor file to HTML.
 *
 * @param filepath - Path to convert
 * @returns The converted file
 */
export const renderAsciidoc = async (filepath: string): Promise<string | Error> => {
  const content = await asciidoc.load(filepath)
  if (content instanceof Error) return content

  const layout = content.getAttribute("layout", "default") as Layout
  return renderTemplate(layout, { title: content.getTitle(), content: content.getContent() })
}

/**
 * Write some content to a directory. Will create a directory and add a `index.html`
 * to it.
 *
 * @param directory - Directory it belongs to
 * @param content - HTML to write
 */
export const writeContent = async (directory: string, content: string | Buffer): Promise<void> => {
  await createDirectory(directory)
  await writeFile(path.join(directory, "index.html"), content)
}

/**
 * Minifies HTML with {@link https://github.com/wilsonzlin/minify-html}.
 *
 * @param source - HTML to minify
 */
export const minifyHTML = (source: string): Buffer => {
  return minify(source, createConfiguration({ minifyJs: false }))
}

/**
 * Clean out the build directory.
 */
export const clean = async (): Promise<void> => {
  await fs.rm(config.out, { recursive: true, force: true })
}

export const writeHTML = (source: string, prod: boolean): Buffer | string => {
  return prod ? minifyHTML(source) : formatHTML(source)
}
