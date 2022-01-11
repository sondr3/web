import { minify } from "csso";
import path from "node:path";
import { EitherAsync } from "purify-ts/EitherAsync.js";
import sass, { CompileResult } from "sass";
import { SourceMapConsumer, SourceMapGenerator } from "source-map";

import { copyFiles, writeFile } from "../utils/fs.js";
import { cacheBust } from "../utils/utils.js";
import { Config } from "./config.js";
import { formatCSS } from "./formatting.js";
import { Site } from "./site.js";

export const _renderStyle = (site: Site): EitherAsync<Error, void> =>
  EitherAsync(async () => {
    const style = await sass.compileAsync(path.join(site.config.assets.styles, "style.scss"), {
      sourceMap: true,
    });
    let hash = cacheBust(style.css, site.config.production);
    const out = await (site.config.production ? optimize(style, hash) : formatCSS(style));
    hash = hash === "" ? `.` : `.${hash}.`;
    const name = `style${hash}css`;

    site.setStyle(name);

    await EitherAsync.sequence([
      writeFile(path.join(site.config.out, name), out.css),
      writeFile(path.join(site.config.out, `${name}.map`), out.map),
    ])
      .mapLeft((e) => console.error(e))
      .run();
  });

/**
 * Renders a given SCSS file to CSS, and optimizing it if running in production
 * mode.
 *
 * @param site - Build configuration
 * @param file - File to render
 * @returns Error if output file could not be written to
 */
export const renderStyles = (site: Site, file: string): EitherAsync<Error, void> =>
  EitherAsync(async () => {
    const style = await sass.compileAsync(path.join(site.config.assets.styles, file), {
      sourceMap: true,
    });

    await writeStyles(site, file, style)
      .mapLeft((error) => error)
      .run();
  });

/**
 * Writes a CSS file and its source map.
 *
 * @param site - Build configuration
 * @param file - CSS filename to write to
 * @param result - Result object from rendering SCSS
 * @returns Error if file creation fails
 */
const writeStyles = (site: Site, file: string, result: CompileResult): EitherAsync<Error, void> =>
  EitherAsync(async () => {
    const hash = cacheBust(result.css, site.config.production);
    const out = await (site.config.production ? optimize(result, hash) : formatCSS(result));
    const name = site.config.production
      ? styleName(site.config, file, `${hash}.css`)
      : styleName(site.config, file);

    site.setStyle(name);

    await EitherAsync.sequence([
      writeFile(path.join(site.config.out, "style.css"), out.css),
      writeFile(path.join(site.config.out, `${name}.map`), out.map),
    ])
      .mapLeft((e) => e)
      .run();
  });

/**
 * Optimize a CSS file by minifying it.
 *
 * @param source - SCSS result object, containing rendered CSS and source map
 * @param hash - Hash given to the CSS file
 * @returns The optimized CSS and its source map
 */
const optimize = async (
  source: CompileResult,
  hash: string,
): Promise<{ css: string; map: string }> => {
  const result = minify(source.css.toString(), {
    filename: "style.scss",
    sourceMap: true,
  });

  const map = result.map as SourceMapGenerator;
  map.applySourceMap(await new SourceMapConsumer(source.sourceMap?.mappings ?? ""), "style.scss");
  const css = result.css + `/*# sourceMappingURL=style${hash}css.map */`;

  return { css, map: map.toString() };
};

/**
 * Converts e.g. `style.css` to `./public/style.abcdefg123.css`.
 *
 * @param config - Build configuration
 * @param file - Filename to correct
 * @param extension - File extension
 * @returns The corrected file extension
 */
export const styleName = (config: Config, file: string, extension = "css"): string => {
  const { name } = path.parse(file);
  return `${config.out}/${name}.${extension}`;
};

export const copyAssets = (site: Site): EitherAsync<Error, void> =>
  copyFiles(site.config.assets.root, site.config.out);
