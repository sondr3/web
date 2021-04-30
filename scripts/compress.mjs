import { createBrotliCompress, createGzip } from "zlib"
import { promises as fs, createReadStream, createWriteStream } from "fs"
import { EventEmitter } from "events"
import stream from "stream"
import path from "path"
import util from "util"

EventEmitter.defaultMaxListeners = 0

const pipeline = util.promisify(stream.pipeline)

const INVALID_EXT = [".map", ".txt", ".scss", ".gz", ".br", ""]
const IGNORE_FILE = ["apple-touch-icon.png", "favicon.ico", "icon-192.png", "icon-512.png", "icon.svg"]

async function readdirRecursive(directory, filepaths = []) {
  const files = await fs.readdir(directory)

  for (const filename of files) {
    const filepath = path.join(directory, filename)
    const stat = await fs.stat(filepath)

    if (stat.isDirectory()) {
      await readdirRecursive(filepath, filepaths)
    } else if (!INVALID_EXT.includes(path.extname(filename)) && !IGNORE_FILE.includes(path.basename(filename))) {
      filepaths.push(filepath)
    }
  }

  return filepaths
}

async function compress(files, ext, compressor) {
  const compressed = files.map((file) => {
    const source = createReadStream(file)
    const destination = createWriteStream(`${file}.${ext}`)
    return pipeline(source, compressor, destination)
  })

  await Promise.allSettled(compressed)
}

async function main() {
  const files = await readdirRecursive(path.join(process.cwd(), "public"))
  if (process.env.CI) files.forEach((file) => console.log(path.basename(file)))

  await compress(files, "br", createBrotliCompress())
  await compress(files, "gz", createGzip({ level: 9 }))
}

await main()
