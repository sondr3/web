import { Logger } from "./logging/Logger"

export const allOk = (...res: (void | Error)[]): boolean => res.every((r) => !(r instanceof Error))

export const asyncTryCatch = async <T>(f: () => Promise<T>, logger: Logger): Promise<T | Error> => {
  try {
    return await f()
  } catch (e) {
    logger.error(e)
    throw e
  }
}

export const slugify = (url: string): string => {
  return url
    .trim()
    .toLowerCase()
    .split(" ")
    .map((w) => w.replace(/[^a-z0-9+]+/gi, ""))
    .join("-")
    .replace(/--+/g, "-")
}
