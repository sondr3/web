import { Config } from "../config"
import { copyFiles, FSError } from "../utils/"
import path from "path"
import { promises as fs } from "fs"
import { logging } from "../logging"
import { EitherAsync } from "purify-ts/EitherAsync"

const logger = logging.getLogger("assets")

/**
 * Copies static assets to the output directory
 */
export const copyAssets = (config: Config): EitherAsync<FSError, void> =>
  EitherAsync(async () => {
    logger.debug("Copying assets")

    // Copying static assets
    await fs.rmdir(path.join(config.out, "images"), { recursive: true })
    await fs.rmdir(path.join(config.out, "assets/scss"), { recursive: true })
    await fs.rmdir(path.join(config.out, "js"), { recursive: true })

    copyFiles(config.assets.images, path.join(config.out, "images"))
      .chain(() => copyFiles(config.assets.style, path.join(config.out, "assets/scss")))
      .chain(() => copyFiles(config.assets.js, path.join(config.out, "js")))
      .mapLeft((error) => error)

    logger.debug("Copying assets finished")
  })
