import {
  createFileHash,
  copyFiles,
  createDirectory,
  dirWalk,
  readFile,
  writeFile,
  readdirRecursive,
  copyFile,
  FSError,
} from "./fs"
import { promises as fs } from "fs"
import path from "path"
import * as os from "os"
import { getConfig } from "../config"
import { Left, Right } from "purify-ts"

describe("dirWalk", () => {
  it("JSON files without recursing", async () => {
    const files = await dirWalk("./", "json", false)

    expect(files).toStrictEqual(["package.json", "renovate.json", "tsconfig.json"])
  })

  it("TS files with recursing", async () => {
    const files = await dirWalk("./lib", "ts")

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
    expect(await createDirectory("/tmp/testing").run()).toEqual(Right(void {}))
  })

  it("cannot create a root directory", async () => {
    expect(await createDirectory("/test").run()).toEqual(Left(new FSError("EACCES: permission denied, mkdir '/test'")))
  })
})

describe("writeFile", () => {
  it("can create a test file", async () => {
    const dir = await fs.mkdtemp(path.join(os.tmpdir(), "test-"))
    const filename = `${dir}/test.txt`
    expect(await writeFile(filename, "hello").run()).toEqual(Right(void {}))
  })

  it("cannot write to /", async () => {
    expect(await writeFile("/test", "hello").run()).toEqual(
      Left(new FSError("EACCES: permission denied, open '/test'")),
    )
  })
})

describe("cacheBustFile", () => {
  it("can add a hash to a file", async () => {
    const actual = await createFileHash(path.resolve(process.cwd(), ".eslintignore"))
    expect(actual).toMatch("e94b497a")
  })
})

describe("copyFiles", () => {
  it("copies files without recursing", async () => {
    const config = getConfig()

    await fs.rmdir(path.join(config.out, "images"), { recursive: true })
    expect(await copyFiles(config.assets.images, path.join(config.out, "images"), false)).toEqual(Right(void {}))
  })

  it("copies files recursively", async () => {
    const dir = await fs.mkdtemp(path.join(os.tmpdir(), "test-"))
    expect(await copyFiles(path.resolve(process.cwd(), "lib"), dir, true).run()).toEqual(Right(void {}))
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
    const dir = await fs.mkdtemp(path.join(os.tmpdir(), "test-copy"))
    await copyFile(path.join(config.assets.root, "robots.txt"), path.join(dir, "robots.txt"))
    expect((await fs.readFile(path.join(dir, "robots.txt"))).toString()).toContain("# www.robotstxt.org/")

    await copyFile(path.join(config.assets.root, "humans.txt"), path.join(dir, "robots.txt"))
    expect((await fs.readFile(path.join(dir, "robots.txt"))).toString()).toContain("/* TEAM */")
  })

  it("copies and does not overwrite", async () => {
    const dir = await fs.mkdtemp(path.join(os.tmpdir(), "test-copy2"))
    await copyFile(path.join(config.assets.root, "robots.txt"), path.join(dir, "robots.txt"))

    const res = await copyFile(path.join(config.assets.root, "humans.txt"), path.join(dir, "robots.txt"), false).run()
    expect(res.isLeft()).toBeTruthy()
    expect((await fs.readFile(path.join(dir, "robots.txt"))).toString()).toContain("# www.robotstxt.org/")
  })
})

describe("readFile", () => {
  it("read files", async () => {
    expect(await readFile(path.resolve(process.cwd(), "package.json")).run()).toBeDefined()
  })

  it("throws when reading unknown file", async () => {
    const res = await readFile(path.resolve(process.cwd(), "poop.json")).run()
    expect(res.isLeft()).toBeTruthy()
  })
})
