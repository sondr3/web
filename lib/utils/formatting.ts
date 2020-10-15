import prettier, { BuiltInParserName as parserName } from "prettier"
import { Result } from "sass"

export const formatHTML = (source: string): string => formatFile(source, "html")

export const formatCSS = (source: Result): { css: string; map: string } => {
  return { css: formatFile(source.css.toString(), "css"), map: source.map?.toString() ?? "" }
}

const formatFile = (source: string, parser: parserName): string => {
  return prettier.format(source, { parser: parser })
}
