import parcel from "@parcel/css";
import swc from "@swc/core";
import { promises as fs } from "node:fs";
import path from "node:path";
import sass, { CompileResult } from "sass";
import { SourceMapConsumer, SourceMapGenerator } from "source-map-js";

import { Context } from "../context.js";
import { Duration } from "../utils/duration.js";
import { copyFiles, walkDir } from "../utils/fs.js";
import * as logger from "../utils/logger.js";
import { cacheBust } from "../utils/utils.js";

export const renderStyles = async ({ site, config }: Context): Promise<void> => {
  const styleDuration = new Duration();
  const style = await sass.compileAsync(path.join(config.assets.styles, "style.scss"), {
    sourceMap: true,
  });
  let result: { name: string; css: string; sourceMap: string };
  if (config.production) {
    const hash = cacheBust(style.css, config.production);
    const name = `style.${hash}.css`;
    result = { ...optimize(style, path.join(config.out, name)), name };
  } else {
    result = {
      name: `style.css`,
      css: style.css,
      sourceMap: buildSourceMap(style),
    };
  }

  site.setStyle(result.name);
  result.css += `/*# sourceMappingURL=/${result.name}.map */`;

  await Promise.allSettled([
    fs.writeFile(path.join(config.out, result.name), result.css),
    fs.writeFile(path.join(config.out, `${result.name}.map`), result.sourceMap),
  ]);

  styleDuration.end();
  logger.info("Finished rendering styles in", styleDuration.result());
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

export const copyAssets = async ({ config }: Context): Promise<void> => {
  return await copyFiles(config.assets.root, config.out);
};

export const compileJs = async ({ site, config }: Context): Promise<void> => {
  for await (const file of walkDir(config.assets.js, () => true)) {
    await site.addJs(file, config.production);
  }

  for (const [file, out] of site.js.entries()) {
    const content = await fs.readFile(path.join(config.assets.js, file));
    if (config.production) {
      const { code } = await swc.minify(content.toString(), { compress: true, mangle: true });
      await fs.writeFile(path.join(config.out, "js", out), code);
    } else {
      await fs.writeFile(path.join(config.out, "js", out), content);
    }
  }
};
