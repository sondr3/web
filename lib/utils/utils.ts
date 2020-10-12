export const allOk = (...res: (void | Error)[]): boolean => res.every((r) => !(r instanceof Error));
