import parcel from "@parcel/css";
import path from "node:path";
import sass, { CompileResult } from "sass";
import { SourceMapConsumer, SourceMapGenerator } from "source-map-js";

import { copyFiles, writeFile } from "../utils/fs.js";
import { cacheBust } from "../utils/utils.js";
import { formatFile } from "./formatting.js";
import { Site } from "./site.js";

export const renderStyles = async (site: Site): Promise<Error | void> => {
  const style = await sass.compileAsync(path.join(site.config.assets.styles, "style.scss"), {
    sourceMap: true,
  });
  let result: { name: string; css: string; sourceMap: string };
  if (site.config.production) {
    const hash = cacheBust(style.css, site.config.production);
    const name = `style.${hash}.css`;
    result = { ...optimize(style, path.join(site.config.out, name)), name };
  } else {
    result = {
      name: `style.css`,
      css: formatFile(style.css, "css", false),
      sourceMap: buildSourceMap(style),
    };
  }

  site.setStyle(result.name);
  result.css += `/*# sourceMappingURL=/${result.name}.map */`;

  const res = await Promise.all([
    writeFile(path.join(site.config.out, result.name), result.css),
    writeFile(path.join(site.config.out, `${result.name}.map`), result.sourceMap),
  ]);

  if (res.some((r) => r instanceof Error)) {
    return new Error("Error when building styles");
  }
};

const buildSourceMap = ({ sourceMap }: CompileResult): string => {
  if (sourceMap === undefined) return "";

  const consumer = new SourceMapConsumer(sourceMap);
  const generator = SourceMapGenerator.fromSourceMap(consumer);

  return generator.toString();
};

/**
 * Optimize a CSS file by minifying it.
 *
 * @param source - SCSS result object, containing rendered CSS and source map
 * @param filename - Name of output file
 * @returns The optimized CSS and its source map
 */
const optimize = (source: CompileResult, filename: string): { css: string; sourceMap: string } => {
  const { code, map } = parcel.transform({
    filename,
    code: Buffer.from(source.css),
    minify: true,
    sourceMap: true,
  });

  return { css: code.toString(), sourceMap: map?.toString() ?? "" };
};

export const copyAssets = (site: Site): Promise<Error | void> => {
  return copyFiles(site.config.assets.root, site.config.out);
};
