import { getConfig } from "../config"
import { copyFiles } from "../utils/fs"
import path from "path"
import { promises as fs } from "fs"
import { logging } from "../utils/logging"

const logger = logging.getLogger("assets")
const config = getConfig()

export const copyAssets = async (): Promise<void> => {
  logger.debug("Copying assets")

  // Copying static assets
  await fs.rmdir(path.join(config.out, "static"), { recursive: true })
  await copyFiles(config.assets.static, path.join(config.out, "static"))

  // Copying SCSS files to sourcemaps work
  await fs.rmdir(path.join(config.out, "assets/scss"), { recursive: true })
  await copyFiles(config.assets.style, path.join(config.out, "assets/scss"))

  // Copy JS
  await fs.rmdir(path.join(config.out, "js"), { recursive: true })
  await copyFiles(config.assets.js, path.join(config.out, "js"))

  logger.debug("Copying assets finished")
}
