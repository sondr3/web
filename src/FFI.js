import sass from "sass";
import { createHash as createHashImpl } from "node:crypto";
import css from "@parcel/css";
import esbuild from "esbuild";

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

export function optimizeJS(code) {
  return function() {
    const res = esbuild.transformSync(code, {
      minify: true,
      logLevel: "warning",
      format: "iife",
    });

    return res.code;
  };
}
