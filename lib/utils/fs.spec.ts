import { cacheBustFile, copyFiles, createDirectory, dirWalk, writeFile } from "./fs";
import assert from "assert";
import { promises as fs } from "fs";
import path from "path";
import * as os from "os";
import { getConfig } from "../config";

describe("dirWalk", () => {
  it("JSON files without recursing", async () => {
    const files = await dirWalk("./", "json", false);

    expect(files).toStrictEqual(["package.json", "renovate.json", "tsconfig.json"]);
  });

  it("TS files with recursing", async () => {
    const files = await dirWalk("./lib", "ts");

    expect(files).toContain("lib/utils/fs.ts");
    expect(files).toContain("lib/utils/fs.spec.ts");
  });
});

describe("createDirectory", () => {
  it("can create a test directory", async () => {
    const res = await createDirectory("/tmp/testing");
    expect(res).toBeUndefined();
  });

  it("cannot create a root directory", async () => {
    const res = await createDirectory("/test");
    expect(res).not.toBeUndefined();
    assert(res !== undefined);
    expect(res.constructor.name).toBe("Error");
  });
});

describe("writeFile", () => {
  it("can create a test file", async () => {
    const dir = await fs.mkdtemp(path.join(os.tmpdir(), "test-"));
    const filename = `${dir}/test.txt`;
    const res = await writeFile(filename, "hello");
    expect(res).toBeUndefined();
  });

  it("cannot write to /", async () => {
    await expect(writeFile("/test", "hello")).rejects.toThrow();
  });
});

describe("cacheBustFile", () => {
  it("can add a hash to a file", () => {
    const actual = cacheBustFile("hello, world", "hello.css");
    expect(actual).toMatch("hello.e4d7f1b4.css");
  });
});

describe("copyFiles", () => {
  it("copies files without recursing", async () => {
    process.env.NODE_ENV = "test";
    const config = getConfig();

    await fs.rmdir(path.join(config.out, "static"), { recursive: true });
    const res = await copyFiles(config.assets.static, path.join(config.out, "static"), false);
    expect(res).toBeUndefined();
  });

  it("copies files recursively", async () => {
    const dir = await fs.mkdtemp(path.join(os.tmpdir(), "test-"));
    await expect(copyFiles(path.resolve(process.cwd(), "lib"), dir, true)).resolves.toBeUndefined();
  });

  it("crashes on illegal directory", async () => {
    await expect(copyFiles("/asdasd", "./")).rejects.toThrow();
  });

  it("cannot copy wrong files", async () => {
    await expect(copyFiles(path.resolve(process.cwd()), "/")).rejects.toThrow();
  });
});
