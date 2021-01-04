import { Logger } from "../logging"

/**
 * Checks if all the supplied results did not error.
 *
 * @param res - List of results from functions
 * @returns `true` if no result is an error, false otherwise
 */
export const allOk = (...res: (void | Error)[]): boolean => res.every((r) => !(r instanceof Error))

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

/**
 * Utility wrapper around {@link https://gigobyte.github.io/purify/adts/EitherAsync#throwE | throwE} that
 * logs and throws an error.
 *
 * @param e - Error type
 * @param throwE - Thrower
 * @param logger - Logging framework
 */
export const throwELog = <E extends Error>(e: E, throwE: (e: E) => never, logger: Logger): never => {
  logger.error(e.message)
  return throwE(e)
}
