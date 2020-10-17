import prettier, { BuiltInParserName as parserName } from "prettier"
import { Result } from "sass"

/**
 * Runs {@link https://prettier.io/ | Prettier} on a HTML string.
 *
 * @param source - HTML source to prettify
 * @returns The prettified HTML
 */
export const formatHTML = (source: string): string => formatFile(source, "html")

/**
 * Runs {@link https://prettier.io/ | Prettier} on a SCSS result.
 *
 * @param source - SCSS result containing CSS and source map
 * @returns The prettified CSS and the source map
 */
export const formatCSS = (source: Result): { css: string; map: string } => {
  return { css: formatFile(source.css.toString(), "css"), map: source.map?.toString() ?? "" }
}

/**
 * Wrapper function to invoke {@link https://prettier.io/ | Prettier} to format files.
 *
 * @param source - File content to format
 * @param parser - Parser to use
 * @returns The formatted string
 */
const formatFile = (source: string, parser: parserName): string => {
  return prettier.format(source, { parser: parser })
}
