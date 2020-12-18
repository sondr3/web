import { createDirectory, dirWalk, writeFile } from "./utils/fs"
import { getConfig } from "./config"
import path from "path"
import { logging } from "./utils/logging"
import { Layout, renderTemplate } from "./templating"
import { copyAssets, renderStyles } from "./assets"
import { Asciidoc } from "./Asciidoc"
import * as pages from "./templates/pages"
import { formatHTML } from "./utils/formatting"
import { Duration } from "./utils/Duration"
import { minify, createConfiguration } from "@minify-html/js"

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
    const rendered = await renderAsciidoc(page, prod)
    if (rendered instanceof Error) return rendered
    const file = path.parse(page)
    await writeContent(path.resolve(config.out, file.name), prod ? minifyHTML(rendered) : formatHTML(rendered))
  }
}

/**
 * Renders "special" pages, e.g. landing page, 404 and such.
 *
 * @param prod - Whether to optimize output
 */
export const renderSpecialPages = async (prod: boolean): Promise<void> => {
  await writeContent(config.out, prod ? minifyHTML(pages.landing()) : formatHTML(pages.landing()))
  await writeFile(path.join(config.out, "404.html"), prod ? minifyHTML(pages.notFound()) : formatHTML(pages.notFound()))
  await writeContent(
    path.resolve(config.out, "404/"),
    prod ? minifyHTML(pages.notFound()) : formatHTML(pages.notFound()),
  )
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
 * @param prod - Formats output if not in prod
 * @returns The converted file
 */
export const renderAsciidoc = async (filepath: string, prod: boolean): Promise<string | Error> => {
  const content = await asciidoc.load(filepath)
  if (content instanceof Error) return content

  const layout = content.getAttribute("layout", "default") as Layout
  const rendered = renderTemplate(layout, { title: content.getTitle(), content: content.getContent() })
  if (!prod) return formatHTML(rendered)

  return rendered
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
