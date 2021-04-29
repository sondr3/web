import { createBrotliCompress, createGzip } from "zlib"
import fs, { createReadStream, createWriteStream } from "fs"
import path from "path"

const INVALID_EXT = [".map", ".txt", ".scss", ".gz", ".br", ""]
const IGNORE_FILE = ["apple-touch-icon.png", "favicon.ico", "icon-192.png", "icon-512.png", "icon.svg"]

export function readdirRecursive(directory, ignored_extension, filepaths = []) {
  const files = fs.readdirSync(directory)

  for (const filename of files) {
    const filepath = path.join(directory, filename)
    const stat = fs.statSync(filepath)

    console.log(path.basename(filename))

    if (stat.isDirectory()) {
      readdirRecursive(filepath, ignored_extension, filepaths)
    } else if (!ignored_extension.includes(path.extname(filename)) && !IGNORE_FILE.includes(path.basename(filename))) {
      filepaths.push(filepath)
    }
  }

  return filepaths
}

function gzip() {
  const files = readdirRecursive(path.join(process.cwd(), "public"), INVALID_EXT)

  for (const file of files) {
    const source = createReadStream(file)
    const destination = createWriteStream(`${file}.gz`)
    const gzip = createGzip({ level: 9 })
    source
      .pipe(gzip)
      .on("error", (err) => console.error(err))
      .pipe(destination)
      .on("error", (err) => console.error(err))
  }
}

function brotli() {
  const files = readdirRecursive(path.join(process.cwd(), "public"), INVALID_EXT)

  for (const file of files) {
    const source = createReadStream(file)
    const destination = createWriteStream(`${file}.br`)
    const brotli = createBrotliCompress()
    source
      .pipe(brotli)
      .on("error", (err) => console.error(err))
      .pipe(destination)
      .on("error", (err) => console.error(err))
  }
}

function main() {
  gzip()
  brotli()
}

main()
