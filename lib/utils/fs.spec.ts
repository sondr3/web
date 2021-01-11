import { promises as fs } from "fs"
import * as os from "os"
import path from "path"
import { Left, Right } from "purify-ts"

import { getConfig } from "../config"
import {
  copyFile,
  copyFiles,
  createDirectory,
  createFileHash,
  FSError,
  readdirRecursive,
  readFile,
  walkDirectory,
  writeFile,
} from "./fs"

describe("walkDirectory", () => {
  it("JSON files without recursing", async () => {
    const files = await walkDirectory("./", "json", false)

    expect(files).toStrictEqual(["package-lock.json", "package.json", "renovate.json", "tsconfig.json"])
  })

  it("TS files with recursing", async () => {
    const files = await walkDirectory("./lib", "ts")

    expect(files).toContain("lib/utils/fs.ts")
    expect(files).toContain("lib/utils/fs.spec.ts")
  })
})

describe("readdirRecursive", () => {
  it("finds all files in directory, ignoring nothing", async () => {
    const files = await readdirRecursive("./assets", [])

    expect(files).toContain("assets/scss/style.scss")
    expect(files).toContain("assets/js/livereload.js")
    expect(files).toContain("assets/images/developer.svg")
  })

  it("finds all files in directory, ignoring scss", async () => {
    const files = await readdirRecursive("./assets", [".scss"])

    expect(files).not.toContain("assets/scss/style.scss")
    expect(files).toContain("assets/js/livereload.js")
    expect(files).toContain("assets/images/developer.svg")
  })
})

describe("createDirectory", () => {
  it("can create a test directory", async () => {
    expect(await createDirectory("/tmp/testing").run()).toEqual(Right(true))
  })

  it("cannot create a root directory", async () => {
    expect(await createDirectory("/test").run()).toEqual(Left(new FSError("EACCES: permission denied, mkdir '/test'")))
  })
})

describe("writeFile", () => {
  it("can create a test file", async () => {
    const directory = await fs.mkdtemp(path.join(os.tmpdir(), "test-"))
    const filename = `${directory}/test.txt`
    expect(await writeFile(filename, "hello").run()).toEqual(Right(true))
  })

  it("cannot write to /", async () => {
    expect(await writeFile("/test", "hello").run()).toEqual(
      Left(new FSError("EACCES: permission denied, open '/test'")),
    )
  })
})

describe("cacheBustFile", () => {
  it("can add a hash to a file", async () => {
    const actual = await createFileHash(path.resolve(process.cwd(), ".eslintignore")).run()
    expect(actual).toEqual(Right("e94b497a"))
  })
})

describe("copyFiles", () => {
  it("copies files without recursing", async () => {
    const config = getConfig()

    await fs.rmdir(path.join(config.out, "images"), { recursive: true })
    expect(await copyFiles(config.assets.images, path.join(config.out, "images"), false)).toEqual(Right(true))
  })

  it("copies files recursively", async () => {
    const directory = await fs.mkdtemp(path.join(os.tmpdir(), "test-"))
    expect(await copyFiles(path.resolve(process.cwd(), "lib"), directory, true).run()).toEqual(Right(true))
  })

  it("crashes on illegal directory", async () => {
    expect(await copyFiles("/asdasd", "./").run()).toEqual(
      Left(new FSError("ENOENT: no such file or directory, scandir '/asdasd'")),
    )
  })

  it("cannot copy wrong files", async () => {
    expect(await copyFiles(path.resolve(process.cwd()), "/").run()).toEqual(
      Left(
        new FSError("EACCES: permission denied, copyfile '/home/sondre/Code/web/.dockerignore' -> '/.dockerignore'"),
      ),
    )
  })
})

describe("copyFile", () => {
  const config = getConfig()

  it("copies and overwrites files by default", async () => {
    const directory = await fs.mkdtemp(path.join(os.tmpdir(), "test-copy"))
    expect(
      await copyFile(path.join(config.assets.root, "robots.txt"), path.join(directory, "robots.txt")).run(),
    ).toEqual(Right(true))
    expect((await fs.readFile(path.join(directory, "robots.txt"))).toString()).toContain("# www.robotstxt.org/")

    expect(
      await copyFile(path.join(config.assets.root, "humans.txt"), path.join(directory, "robots.txt")).run(),
    ).toEqual(Right(true))
    expect((await fs.readFile(path.join(directory, "robots.txt"))).toString()).toContain("/* TEAM */")
  })

  it("copies and does not overwrite", async () => {
    const directory = await fs.mkdtemp(path.join(os.tmpdir(), "test-copy2"))
    expect(
      await copyFile(path.join(config.assets.root, "robots.txt"), path.join(directory, "robots.txt")).run(),
    ).toEqual(Right(true))

    const result = await copyFile(
      path.join(config.assets.root, "humans.txt"),
      path.join(directory, "robots.txt"),
      false,
    ).run()
    expect(result.isLeft()).toBeTruthy()
    expect((await fs.readFile(path.join(directory, "robots.txt"))).toString()).toContain("# www.robotstxt.org/")
  })
})

describe("readFile", () => {
  it("read files", async () => {
    expect(await readFile(path.resolve(process.cwd(), "package.json")).run()).toBeDefined()
  })

  it("throws when reading unknown file", async () => {
    const result = await readFile(path.resolve(process.cwd(), "poop.json")).run()
    expect(result.isLeft()).toBeTruthy()
  })
})
