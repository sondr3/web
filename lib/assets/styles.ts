import csso from "csso"
import path from "path"
import { EitherAsync } from "purify-ts/EitherAsync"
import sass, { Result as SassResult } from "sass"
import { SourceMapConsumer, SourceMapGenerator } from "source-map"
import { CustomError } from "ts-custom-error"

import { logging } from "../logging"
import { Config, Site } from "../site"
import { cacheBust, createDirectory, formatCSS, prettyPrintDuration, writeFile } from "../utils"

const logger = logging.getLogger("sass")

export class StyleError extends CustomError {
  public constructor(message: string) {
    super(message)
  }
}

/**
 * Renders a given SCSS file to CSS, and optimizing it if running in production
 * mode.
 *
 * @param site - Build configuration
 * @param file - File to render
 * @returns Error if output file could not be written to
 */
export const renderStyles = (site: Site, file: string): EitherAsync<StyleError, void> =>
  EitherAsync(async () => {
    logger.debug(`Rendering ${file}`)
    const style = sass.renderSync({
      file: file,
      sourceMap: true,
      outFile: styleName(site.config, file),
    })

    logger.debug(`Rendered ${file}: took ${prettyPrintDuration(style.stats.duration)}`)

    await writeStyles(site, file, style)
      .mapLeft((error) => new StyleError(error.message))
      .run()
  })

/**
 * Writes a CSS file and its source map.
 *
 * @param site - Build configuration
 * @param file - CSS filename to write to
 * @param result - Result object from rendering SCSS
 * @returns Error if file creation fails
 */
const writeStyles = (site: Site, file: string, result: SassResult): EitherAsync<StyleError, void> =>
  EitherAsync(async () => {
    const parsed = path.parse(file)

    const hash = cacheBust(result.css, site.config.production)
    const out = await (site.config.production ? optimize(result, file, hash) : formatCSS(result))
    const name = site.config.production ? styleName(site.config, file, `${hash}.css`) : styleName(site.config, file)

    await EitherAsync.sequence([
      createDirectory(parsed.dir),
      writeFile(name, out.css),
      writeFile(`${name}.map`, out.map),
    ])
      .mapLeft((error) => new StyleError(error.message))
      .run()

    site.state.styles.set(`${parsed.name}.css`, name)
  })

/**
 * Optimize a CSS file by minifying it.
 *
 * @param source - SCSS result object, containing rendered CSS and source map
 * @param file - Filename, used to create correct production source map
 * @param hash - Hash given to the CSS file
 * @returns The optimized CSS and its source map
 */
const optimize = async (source: SassResult, file: string, hash: string): Promise<{ css: string; map: string }> => {
  const result = csso.minify(source.css.toString(), {
    filename: file,
    sourceMap: true,
  })

  const map = result.map as SourceMapGenerator
  map.applySourceMap(await new SourceMapConsumer(source.map?.toString() ?? ""), file)
  const css = result.css + `/*# sourceMappingURL=style.${hash}.css.map */`

  return { css, map: map.toString() }
}

/**
 * Converts e.g. `style.css` to `./public/style.abcdefg123.css`.
 *
 * @param config - Build configuration
 * @param file - Filename to correct
 * @param extension - File extension
 * @returns The corrected file extension
 */
export const styleName = (config: Config, file: string, extension = "css"): string => {
  const { name } = path.parse(file)
  return `${config.out}/${name}.${extension}`
}
