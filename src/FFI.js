import sass from "sass";
import { createHash as createHashImpl } from "node:crypto";
import css from "@parcel/css";

const sassCompile = sass.compile;

export { cpSync as cpSyncImpl, copyFileSync as copyFileSyncImpl } from "node:fs";
export { sassCompile as sassCompileImpl };

export function createHash(input) {
  return createHashImpl("md5").update(input).digest("hex");
}

export function optimizeCSS(filename) {
  return function(input) {
    return function(minify) {
      return function() {
        const { code, map } = css.transform({
          filename,
          code: Buffer.from(input),
          minify,
          sourceMap: true,
        });

        return {
          code: code.toString("utf8"),
          map: map.toString("utf8"),
        };
      };
    };
  };
}
