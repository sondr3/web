import sass, { Result as SassResult } from "sass"
import { logging } from "../utils/logging"
import path from "path"
import { createDirectory, writeFile } from "../utils/fs"
import { getConfig } from "../config"
import { allOk } from "../utils/utils"
import { siteState } from "../state"

const state = siteState
const logger = logging.getLogger("sass")

export const renderStyles = (file: string, prod: boolean): Promise<void | Error> => {
  const style = sass.renderSync({
    file: file,
    sourceMap: !prod,
    outFile: styleName(file),
  })

  logger.debug(`Rendered ${file}: took ${style.stats.duration}`)

  return writeStyles(file, style)
}

const writeStyles = async (file: string, res: SassResult): Promise<void | Error> => {
  const parsed = path.parse(file)
  const dir = await createDirectory(parsed.dir)
  const css = await writeFile(styleName(file), res.css)
  const map = await writeFile(styleName(file, "css.map"), res.map ?? "")

  if (!allOk(...[dir, css, map])) return new Error("Could not create styles")
  state.styles.set(`${parsed.name}.css`, styleName(file, "css"))

  return
}

export const styleName = (file: string, ext: string = "css"): string => {
  const config = getConfig()
  const { name } = path.parse(file)
  return `${config.out}/${name}.${ext}`
}
