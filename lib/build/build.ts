import { promises as fs } from "fs"
import path from "path"
import { EitherAsync } from "purify-ts/EitherAsync"
import { CustomError } from "ts-custom-error"

import { copyAssets, renderStyles } from "../assets"
import { Config } from "../config"
import { renderPages, renderSpecialPages } from "../content"
import { sitemap } from "../content/sitemap"
import { logging } from "../logging"
import { copyFile, Duration } from "../utils"
import { compress } from "."

const logger = logging.getLogger("build")

export class BuildError extends CustomError {
  public constructor(message: string) {
    super(message)
  }
}
/**
 * Build the whole site by copying assets, building styles and all pages, posts etc.
 *
 * @param config - Configuration to build site with
 * @param production - Whether to optimize output
 */
export const buildSite = (config: Config, production: boolean): EitherAsync<BuildError, void> =>
  EitherAsync(async () => {
    logger.log(`Building site ${config.meta.title} (${config.meta.url})`)
    const duration = new Duration()

    await copyAssets(config)
      .chain(() => renderStyles(path.join(config.assets.style, "style.scss"), production))
      .chain(() => renderPages(production))
      .mapLeft((error) => new BuildError(error.message))
      .run()

    await renderSpecialPages(production)
    await createRootFiles(config)
    await sitemap()
    await compress(production)
    duration.end()
    logger.log(`Took ${duration.result()} to build site`)
  })

/**
 * Create assorted files that are often found in the root of webpages, e.g.
 * `robots.txt` and so on.
 */
export const createRootFiles = async (config: Config): Promise<void> => {
  const files = [
    "robots.txt",
    "humans.txt",
    "apple-touch-icon.png",
    "favicon.ico",
    "icon.svg",
    "icon-192.png",
    "icon-512.png",
    "manifest.webmanifest",
  ].map((file) => copyFile(path.join(config.assets.root, file), path.join(config.out, file)))

  await Promise.allSettled(files)
}

/**
 * Clean out the build directory.
 */
export const clean = async (config: Config): Promise<void> => {
  await fs.rm(config.out, { recursive: true, force: true })
}
