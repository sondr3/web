import { getConfig } from "../config"
import { copyFiles } from "../utils/"
import path from "path"
import { promises as fs } from "fs"
import { logging } from "../logging"

const logger = logging.getLogger("assets")
const config = getConfig()

/**
 * Copies static assets to the output directory
 */
export const copyAssets = async (): Promise<void> => {
  logger.debug("Copying assets")

  // Copying static assets
  await fs.rmdir(path.join(config.out, "images"), { recursive: true })
  await copyFiles(config.assets.images, path.join(config.out, "images"))

  // Copying SCSS files to make sourcemaps work
  await fs.rmdir(path.join(config.out, "assets/scss"), { recursive: true })
  await copyFiles(config.assets.style, path.join(config.out, "assets/scss"))

  // Copy JS
  await fs.rmdir(path.join(config.out, "js"), { recursive: true })
  await copyFiles(config.assets.js, path.join(config.out, "js"))

  logger.debug("Copying assets finished")
}
