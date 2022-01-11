import prettier, { BuiltInParserName as parserName } from "prettier";
import { CompileResult } from "sass";

let formatter: typeof prettier | null = null;

/**
 * Runs {@link https://prettier.io/ | Prettier} on a HTML string.
 *
 * @param source - HTML source to prettify
 * @returns The prettified HTML
 */
export const formatHTML = (source: string): string => formatFile(source, "html");

/**
 * Runs {@link https://prettier.io/ | Prettier} on a SCSS result.
 *
 * @param source - SCSS result containing CSS and source map
 * @returns The prettified CSS and the source map
 */
export const formatCSS = (
  source: CompileResult,
): { readonly css: string; readonly map: string } => {
  return { css: formatFile(source.css, "css"), map: source.sourceMap?.file ?? "" };
};

/**
 * Wrapper function to invoke {@link https://prettier.io/ | Prettier} to format files.
 *
 * @param source - File content to format
 * @param parser - Parser to use
 * @returns The formatted string
 */
const formatFile = (source: string, parser: parserName): string => {
  if (formatter === null) formatter = prettier;
  return formatter.format(source, { parser: parser });
};
