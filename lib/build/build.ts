import { copyFile, Duration } from "../utils/"
import { getConfig } from "../config"
import path from "path"
import { logging } from "../logging"
import { compress } from "./"
import { copyAssets, renderStyles } from "../assets"
import { promises as fs } from "fs"
import { renderPages, renderSpecialPages } from "../content"
import { sitemap } from "../content/sitemap"

const logger = logging.getLogger("build")
const config = getConfig()

/**
 * Build the whole site by copying assets, building styles and all pages, posts etc.
 *
 * @param prod - Whether to optimize output
 */
export const buildSite = async (prod: boolean): Promise<void> => {
  logger.log(`Building site ${config.meta.title} (${config.meta.url})`)
  const duration = new Duration()
  await copyAssets()
  await renderStyles(path.join(getConfig().assets.style, "style.scss"), prod)
  await renderSpecialPages(prod)
  await renderPages(prod)
  await createRootFiles()
  await sitemap()
  await compress(prod)
  duration.end()
  logger.log(`Took ${duration.result()} to build site`)
}

/**
 * Create assorted files that are often found in the root of webpages, e.g.
 * `robots.txt` and so on.
 */
export const createRootFiles = async (): Promise<void> => {
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
export const clean = async (): Promise<void> => {
  await fs.rm(config.out, { recursive: true, force: true })
}
