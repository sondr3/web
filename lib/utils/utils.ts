import { Logger } from "../logging"

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
    .map((w) => w.replace(/[^\d+a-z]+/gi, ""))
    .join("-")
    .replace(/--+/g, "-")
}

interface ThrowELog<E extends Error> {
  error: E
  throwE: (error: E) => never
  logger: Logger
}

/**
 * Utility wrapper around {@link https://gigobyte.github.io/purify/adts/EitherAsync#throwE | throwE} that
 * logs and throws an error.
 *
 * @param error - Error type
 * @param throwE - Thrower
 * @param logger - Logging framework
 */
export const throwELog = <E extends Error>({ error, throwE, logger }: ThrowELog<E>): never => {
  logger.error(error.message)
  return throwE(error)
}
