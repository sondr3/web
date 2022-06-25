import sass from "sass";
import { createHash as createHashImpl } from "node:crypto";

const sassCompile = sass.compile;

export { cpSync as cpSyncImpl, copyFileSync as copyFileSyncImpl } from "node:fs";
export { sassCompile as sassCompileImpl };

export const createHash = (input) => {
  return createHashImpl("md5").update(input).digest("hex");
};
