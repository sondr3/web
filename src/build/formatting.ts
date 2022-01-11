import prettier from "prettier";

/**
 * Wrapper function to invoke {@link https://prettier.io/ | Prettier} to format files.
 *
 * @param source - File content to format
 * @param parser - Parser to use
 * @param production - Skip formatting in production
 * @returns The formatted string
 */
export const formatFile = (source: string, parser: "css" | "html", production: boolean): string => {
  if (production) return source;
  return prettier.format(source, { parser: parser });
};
