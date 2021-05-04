import { brotliCompressSync, gzipSync } from "zlib"
import { promises as fs } from "fs"
import path from "path"

const INVALID_EXT = [".map", ".txt", ".scss", ".gz", ".br", ".png", ".jpg", ""]
const IGNORE_FILE = ["favicon.ico", "icon.svg"]

/**
 * @param {string} directory - Path to recursively look in
 * @param {string[]} filepaths - Array to hold found files
 * @returns {Promise<string[]>} - Files found
 */
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

/**
 * @param {string[]} files - Files to compress
 * @param {string} ext  - Extension for the compressed files
 * @param {function} compressor - Function to compress with
 */
function compress(files, ext, compressor) {
  files.map(async (file) => {
    const content = await fs.readFile(file)
    const compressed = compressor(content)
    await fs.writeFile(`${file}.${ext}`, compressed)
  })
}

const gzip = (content) => gzipSync(content, { level: 9 })

async function main() {
  const start = process.hrtime.bigint()
  const files = await readdirRecursive(path.join(process.cwd(), "public"))
  if (process.env.CI) files.forEach((file) => console.log(file))

  compress(files, "br", brotliCompressSync)
  compress(files, "gz", gzip)
  const end = process.hrtime.bigint()
  console.error(`Compressed ${files.length} files in ${(end - start) / 1000000n}ms`)
}

await main()
