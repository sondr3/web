import path from "path"
import { EitherAsync } from "purify-ts/EitherAsync"

import { Config } from "../config"
import { logging } from "../logging"
import { copyFiles, FSError, rmdirs } from "../utils"

const logger = logging.getLogger("assets")

/**
 * Copies static assets to the output directory
 */
export const copyAssets = (config: Config): EitherAsync<FSError, void> =>
  EitherAsync(async () => {
    logger.debug("Copying assets")

    const paths = [path.join(config.out, "images"), path.join(config.out, "assets/scss"), path.join(config.out, "js")]

    await rmdirs(paths, true).run()
    await EitherAsync.sequence([
      copyFiles(config.assets.images, path.join(config.out, "images")),
      copyFiles(config.assets.style, path.join(config.out, "assets/scss")),
      copyFiles(config.assets.js, path.join(config.out, "js")),
    ])
      .mapLeft((error) => error)
      .run()

    logger.debug("Copying assets finished")
  })
