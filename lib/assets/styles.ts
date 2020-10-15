import sass, { Result as SassResult } from "sass"
import { logging } from "../utils/logging"
import path from "path"
import { createFileHash, createDirectory, writeFile } from "../utils/fs"
import { getConfig } from "../config"
import { allOk } from "../utils/utils"
import { siteState } from "../state"
import { formatCSS } from "../utils/formatting"
import postcss from "postcss"
import cssnano from "cssnano"
import { prettyPrintDuration } from "../utils/Duration"

const state = siteState
const logger = logging.getLogger("sass")

export const renderStyles = async (file: string, prod: boolean): Promise<void | Error> => {
  logger.debug(`Rendering ${file}`)
  const style = sass.renderSync({
    file: file,
    sourceMap: true,
    outFile: await styleName(file),
  })

  logger.debug(`Rendered ${file}: took ${prettyPrintDuration(style.stats.duration)}`)

  return writeStyles(file, style, prod)
}

const writeStyles = async (file: string, res: SassResult, prod: boolean): Promise<void | Error> => {
  const parsed = path.parse(file)

  const hash = prod ? `${await createFileHash(file)}.` : ""
  const out = await (prod ? optimize(res, parsed.name, hash) : formatCSS(res))

  const dir = await createDirectory(parsed.dir)
  const css = await writeFile(await styleName(file, `${hash}css`), out.css)
  const map = await writeFile(await styleName(file, `${hash}css.map`), out.map)

  if (!allOk(...[dir, css, map])) return new Error("Could not create styles")
  state.styles.set(`${parsed.name}.css`, await styleName(file, `${hash}css`))

  return
}

const optimize = async (source: SassResult, file: string, hash: string): Promise<{ css: string; map: string }> => {
  const res = await postcss([cssnano({ preset: "advanced" })]).process(source.css, {
    from: `${file}.${hash}css`,
    map: { inline: false, prev: source.map?.toString() },
  })
  res.warnings().forEach((warn) => logger.warn(warn.toString()))
  return { css: res.css, map: res.map.toString() }
}

export const styleName = async (file: string, ext: string = "css"): Promise<string> => {
  const config = getConfig()
  const { name } = path.parse(file)
  return `${config.out}/${name}.${ext}`
}
