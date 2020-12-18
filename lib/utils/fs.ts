import { promises as fs } from "fs"
import path from "path"
import crypto from "crypto"
import { logging } from "../logging"
import { asyncTryCatch } from "./utils"

const logger = logging.getLogger("fs")

/**
 * Recursively walk directories finding all files matching the extension.
 *
 * @param directory - Directory to walk
 * @param extension - Extensions to look for
 * @param recurse   - Whether to recursively go into folders
 * @param filepaths - Array of found files
 * @returns An array of all found files
 */
export async function dirWalk(
  directory: string,
  extension: string,
  recurse = true,
  filepaths: Array<string> = [],
): Promise<Array<string>> {
  const files = await fs.readdir(directory)

  for (const filename of files) {
    const filepath = path.join(directory, filename)
    const stat = await fs.stat(filepath)

    if (stat.isDirectory() && recurse) {
      await dirWalk(filepath, extension, recurse, filepaths)
    } else if (path.extname(filename) === `.${extension}`) {
      filepaths.push(filepath)
    }
  }

  return filepaths
}

/**
 * A very similar function to {@link dirWalk}, the major difference being that
 * this lists all the content of a directory and its children.
 *
 * @param directory - Directory to find contents of
 * @param filepaths - Array of found files
 * @returns An array of all found files
 */
export async function readdirRecursive(directory: string, filepaths: Array<string> = []): Promise<Array<string>> {
  const files = await fs.readdir(directory)

  for (const filename of files) {
    const filepath = path.join(directory, filename)
    const stat = await fs.stat(filepath)

    if (stat.isDirectory()) {
      await readdirRecursive(filepath, filepaths)
    } else {
      filepaths.push(filepath)
    }
  }

  return filepaths
}

/**
 * Copies all files from a source directory to a destination, optionally recursively
 * copying the subdirectories as well.
 *
 * @param source - Where to copy from
 * @param destination - Where to copy to
 * @param recurse - Whether to recursively copy
 * @returns Error if something went wrong
 */
export const copyFiles = async (source: string, destination: string, recurse = true): Promise<void | Error> => {
  const entries = await fs.readdir(source, { withFileTypes: true })
  const dir = await createDirectory(destination)
  if (dir) return dir

  for (const entry of entries) {
    const src = path.join(source, entry.name)
    const dest = path.join(destination, entry.name)

    if (entry.isDirectory() && recurse) {
      await copyFiles(src, dest, recurse)
    } else {
      const res = await asyncTryCatch(async () => fs.copyFile(src, dest, 1), logger)
      if (res instanceof Error) return res
    }
  }
}

/**
 * Create a directory for the file whose path is supplied, recursively creating the directories
 * required.
 *
 * @param filepath - Path to where file wants to go
 * @returns Error if something goes wrong
 */
export const createDirectory = async (filepath: string): Promise<void | Error> => {
  const res = await asyncTryCatch(async () => fs.mkdir(filepath, { recursive: true }), logger)
  if (res instanceof Error) return res

  return
}

/**
 * Writes some content to a file.
 *
 * @param filepath - File to write to
 * @param content - Content to write
 * @returns Error if writing fails
 */
export const writeFile = async (filepath: string, content: string | Buffer): Promise<void | Error> => {
  return await asyncTryCatch(async () => fs.writeFile(filepath, content), logger)
}

/**
 * Reads the content of a file.
 *
 * @param filepath - File to read contents of
 * @returns Error if file could not be read
 */
export const readFile = async (filepath: string): Promise<string | Error> => {
  return await asyncTryCatch(async () => fs.readFile(filepath, { encoding: "utf-8" }), logger)
}

/**
 * Create a hash of a file, based on its content. The hash is an eight character long
 * MD5 hash used by reading the files contents.
 *
 * @param filepath - File to read and hash
 * @returns The hash of the file
 */
export const createFileHash = async (filepath: string): Promise<string> => {
  const contents = await readFile(filepath)
  if (contents instanceof Error) throw contents
  const md5 = crypto.createHash("md5")
  md5.update(contents)
  return md5.digest("hex").slice(0, 8)
}
