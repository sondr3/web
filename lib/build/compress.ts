import { Config } from "../config"
import stream from "stream/promises"
import { createGzip } from "zlib"
import { readdirRecursive } from "../utils"
import { createReadStream, createWriteStream } from "fs"

const INVALID_EXT = [".map", ".txt", ".scss", ".gz", ".br", ""]

/**
 * Compress a directory and all its files with Gzip.
 *
 * @param config - Sitewide configuration
 */
export async function gzip(config: Config): Promise<void> {
  const files = await readdirRecursive(config.out, INVALID_EXT)

  console.log(files)

  for (const file of files) {
    const source = createReadStream(file)
    const dest = createWriteStream(`${file}.gz`)
    const gzip = createGzip({ level: 9 })

    await stream.pipeline(source, gzip, dest)
  }
}
