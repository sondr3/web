import { Logger } from "../logging"

/**
 * Checks if all the supplied results did not error.
 *
 * @param res - List of results from functions
 * @returns `true` if no result is an error, false otherwise
 */
export const allOk = (...res: (void | Error)[]): boolean => res.every((r) => !(r instanceof Error))

/**
 * A wrapper function around async try/catch blocks that are used extensively in
 * the code base.
 *
 * @param f - Function to wrap
 * @param logger - Logger to output errors to
 * @returns Result from function or throws an error otherwise
 */
export const asyncTryCatch = async <T>(f: () => Promise<T>, logger: Logger): Promise<T | Error> => {
  try {
    return await f()
  } catch (e) {
    logger.error(e)
    throw e
  }
}

/**
 * Convert a title from a page to a slug that can be used in the generated site. Turns
 * `Hello, world!` into `hello-world`.
 *
 * @param title - Title to convert
 * @returns The resulting slug
 */
export const slugify = (title: string): string => {
  return title
    .trim()
    .toLowerCase()
    .split(" ")
    .map((w) => w.replace(/[^a-z0-9+]+/gi, ""))
    .join("-")
    .replace(/--+/g, "-")
}
