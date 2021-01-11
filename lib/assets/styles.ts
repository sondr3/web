import sass, { Result as SassResult } from "sass"
import { logging } from "../logging"
import path from "path"
import { getConfig } from "../config"
import { siteState } from "../state"
import csso from "csso"
import { SourceMapGenerator, SourceMapConsumer } from "source-map"
import { writeFile, prettyPrintDuration, createFileHash, formatCSS, createDirectory, FSError } from "../utils"
import { EitherAsync } from "purify-ts/EitherAsync"

const state = siteState
const logger = logging.getLogger("sass")

/**
 * Renders a given SCSS file to CSS, and optimizing it if running in production
 * mode.
 *
 * @param file - File to render
 * @param prod - Whether to optimize file
 * @returns Error if output file could not be written to
 */
export const renderStyles = (file: string, prod: boolean): EitherAsync<FSError, void> =>
  EitherAsync(async () => {
    logger.debug(`Rendering ${file}`)
    const style = sass.renderSync({
      file: file,
      sourceMap: true,
      outFile: styleName(file),
    })

    logger.debug(`Rendered ${file}: took ${prettyPrintDuration(style.stats.duration)}`)

    await writeStyles(file, style, prod)
      .mapLeft((error) => error)
      .run()
  })

/**
 * Writes a CSS file and its source map.
 *
 * @param file - CSS filename to write to
 * @param res - Result object from rendering SCSS
 * @param prod - Whether to optimize file
 * @returns Error if file creation fails
 */
const writeStyles = (file: string, res: SassResult, prod: boolean): EitherAsync<FSError, void> =>
  EitherAsync(async () => {
    const parsed = path.parse(file)

    const hash = prod ? `${await createFileHash(file)}.` : ""
    const out = await (prod ? optimize(res, file, hash) : formatCSS(res))

    await createDirectory(parsed.dir)
      .chain(() => writeFile(styleName(file, `${hash}css`), out.css))
      .chain(() => writeFile(styleName(file, `${hash}css.map`), out.map))
      .mapLeft((err) => err)

    state.styles.set(`${parsed.name}.css`, styleName(file, `${hash}css`))
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
  const res = csso.minify(source.css.toString(), {
    filename: file,
    sourceMap: true,
  })

  const map = res.map as SourceMapGenerator
  map.applySourceMap(await new SourceMapConsumer(source.map?.toString() ?? ""), file)
  const css = res.css + `/*# sourceMappingURL=style.${hash}css.map */`

  return { css, map: map.toString() }
}

/**
 * Converts e.g. `style.css` to `./public/style.abcdefg123.css`.
 *
 * @param file - Filename to correct
 * @param ext - File extension
 * @returns The corrected file extension
 */
export const styleName = (file: string, ext: string = "css"): string => {
  const config = getConfig()
  const { name } = path.parse(file)
  return `${config.out}/${name}.${ext}`
}
