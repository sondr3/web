import { createConfiguration, minify } from "@minify-html/js"
import { Asciidoctor } from "asciidoctor"
import path from "path"
import { EitherAsync } from "purify-ts/EitherAsync"

import { Asciidoc, Layout, renderTemplate } from "../build"
import { Site } from "../site"
import { createDirectory, formatHTML, FSError, writeFile } from "../utils"

export * from "./pages"

const asciidoc = new Asciidoc()

export type Metadata = {
  readonly title: string
  readonly description: string
  readonly path: string
  readonly createdAt?: Date
  readonly modifiedAt?: Date
}

/**
 * Load and parse a file into a {@Link Asciidoctor.Document}.
 *
 * @param filepath - Path to load
 * @returns The parsed file
 */
export const convertAsciidoc = (filepath: string): EitherAsync<FSError, Asciidoctor.Document> => asciidoc.load(filepath)

/**
 * Renders a Asciidoctor file to HTML.
 *
 * @param site - Build configuration
 * @param content - Asciidoc document
 * @returns The converted file
 */
export const renderAsciidoc = (site: Site, content: Asciidoctor.Document): string => {
  const layout = content.getAttribute("layout", "default") as Layout
  return renderTemplate(site, layout, { title: content.getTitle(), content: content.getContent() })
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
  return minify(source, createConfiguration({ minifyJs: false, minifyCss: false }))
}

/**
 * Either prettifies the HTML or minifies it, it's essentially a small utility
 * function around {@link minifyHTML} and {@link formatHTML}.
 *
 * @param source - HTML to format/minify
 * @param production - Whether we are in production mode
 * @returns The modified HTML
 */
export const writeHTML = (source: string, production: boolean): Buffer | string => {
  return production ? minifyHTML(source) : formatHTML(source)
}
