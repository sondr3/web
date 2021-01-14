import { createConfiguration, minify } from "@minify-html/js"
import { Asciidoctor } from "asciidoctor"
import path from "path"
import { EitherAsync } from "purify-ts/EitherAsync"
import YAML from "yaml"

import { Asciidoc, BuildError, Layout } from "../build"
import { Site } from "../site"
import type { PartialStringyTyped } from "../utils"
import { createDirectory, formatHTML, writeFile } from "../utils"
import { convertDate } from "./helpers"
import { renderPages, renderSpecialPages } from "./pages"

export type Metadata = {
  readonly path: string
  readonly layout: Layout
}

export type Frontmatter = {
  readonly title: string
  readonly description?: string
  readonly created?: Date
  readonly modified?: Date
}

export type ContentData = {
  readonly metadata: Metadata
  readonly frontmatter: Frontmatter
}

type ParsedData = PartialStringyTyped<Metadata> & PartialStringyTyped<Frontmatter>

export type Content = ContentData & {
  content: Asciidoctor.Document
}

// FIXME: DOCUMENTATION AND TESTS PLZ
export const buildPages = (site: Site): EitherAsync<BuildError, void> =>
  EitherAsync(async () => {
    const asciidoc = new Asciidoc()

    await EitherAsync.sequence([renderPages(site, asciidoc), renderSpecialPages(site)])
      .mapLeft((error) => new BuildError(error.message))
      .run()
  })

export const renderContent = (asciidoc: Asciidoc, content: string, defaultMetadata: Metadata): Content => {
  const frontmatterEnd = findFrontmatter(content)
  const data = parseDocumentData(content, frontmatterEnd)
  const document = asciidoc.parse(content.slice(frontmatterEnd + 4))
  const frontmatter = createFrontmatter(data, document)
  const metadata = createMetadata(data, defaultMetadata)

  return { metadata, frontmatter, content: document }
}

export const findFrontmatter = (input: string): number => {
  const start = input.indexOf("---\n")
  return input.indexOf("---\n", start + 4)
}

/**
 * Parses and converts the frontmatter of a Markdown file.
 *
 * @param input - File to extract frontmatter from
 * @param frontmatterEnd - Where the final `---\n` in the frontmatter is
 * @returns An object of `{ frontmatter, end of frontmatter }`
 */
export const parseDocumentData = (input: string, frontmatterEnd: number): ParsedData => {
  return YAML.parse(input.slice(4, frontmatterEnd)) as ParsedData
}

export const createMetadata = (data: ParsedData, defaults: Metadata): Metadata => {
  return { path: data?.path ?? defaults.path, layout: data?.layout ?? defaults.layout } as Metadata
}

export const createFrontmatter = (data: ParsedData, document: Asciidoctor.Document): Frontmatter => {
  return {
    title: data?.title ?? document.getTitle(),
    description: data?.description,
    created: convertDate(data?.created),
    modified: convertDate(data?.modified),
  }
}

/**
 * Write some content to a directory. Will create a directory and add a `index.html`
 * to it.
 *
 * @param directory - Directory it belongs to
 * @param content - HTML to write
 */
export const writeContent = (directory: string, content: string | Buffer): EitherAsync<BuildError, boolean[]> =>
  EitherAsync.sequence([createDirectory(directory), writeFile(path.join(directory, "index.html"), content)]).mapLeft(
    (error) => new BuildError(error.message),
  )

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
