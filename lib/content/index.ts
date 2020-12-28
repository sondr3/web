import { siteState } from "../state"
import { logging } from "../logging"
import { Asciidoc, Layout, renderTemplate } from "../build"
import { createDirectory, formatHTML, writeFile } from "../utils"
import path from "path"
import { createConfiguration, minify } from "@minify-html/js"
import { Asciidoctor } from "asciidoctor"

export * from "./pages"

const asciidoc = new Asciidoc()
const state = siteState
const logger = logging.getLogger("content")

export interface Metadata {
  title: string
  description: string
  path: string
  createdAt?: Date
  modifiedAt?: Date
}

export const addPage = (data: Metadata): void => {
  logger.debug(`Adding page ${data.title} to state`)
  state.pages.set(data.path, data)
}

/**
 * Load and parse a file into a {@Link Asciidoctor.Document}.
 *
 * @param filepath - Path to load
 * @returns The parsed file
 */
export const convertAsciidoc = async (filepath: string): Promise<Asciidoctor.Document | Error> => {
  return asciidoc.load(filepath)
}

/**
 * Renders a Asciidoctor file to HTML.
 *
 * @param content - Asciidoc document
 * @returns The converted file
 */
export const renderAsciidoc = (content: Asciidoctor.Document): string | Error => {
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
 * Either prettifies the HTML or minifies it, it's essentially a small utility
 * function around {@link minifyHTML} and {@link formatHTML}.
 *
 * @param source - HTML to format/minify
 * @param prod - Whether we are in production mode
 * @returns The modified HTML
 */
export const writeHTML = (source: string, prod: boolean): Buffer | string => {
  return prod ? minifyHTML(source) : formatHTML(source)
}
