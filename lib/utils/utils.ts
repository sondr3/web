import { Logger } from "./logging/Logger";

export const allOk = (...res: (void | Error)[]): boolean => res.every((r) => !(r instanceof Error));

export const asyncTryCatch = async <T>(f: () => Promise<T>, logger: Logger): Promise<T | Error> => {
  try {
    return await f();
  } catch (e) {
    logger.error(e);
    throw e;
  }
};
