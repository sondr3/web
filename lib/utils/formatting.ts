import prettier, { BuiltInParserName as parserName } from "prettier"

export const formatHTML = (source: string): string => formatFile(source, "html")

export const formatCSS = (source: string): string => formatFile(source, "css")

const formatFile = (source: string, parser: parserName): string => {
  return prettier.format(source, { parser: parser })
}
