import { promises as fs } from "fs";
import { cacheBustFile, createDirectory, createFile, dirWalk } from "./fs";
import path from "path";
import * as os from "os";

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
    const res = await createDirectory("/tmp/testing").run();
    expect(res.isRight()).toBeTruthy();
  });

  it("cannot create a root directory", async () => {
    const res = await createDirectory("/test").run();
    expect(res.isLeft()).toBeTruthy();
    expect(res.isRight()).toBeFalsy();
    expect(res.map((r) => r.includes("Could not create directory"))).toBeTruthy();
  });
});

describe("cacheBustFile", () => {
  it("can add a hash to a file", () => {
    const actual = cacheBustFile("hello, world", "hello.css");
    expect(actual).toMatch("hello.e4d7f1b4.css");
  });
});

describe("createFile", () => {
  it("creates a new empty file in a new folder", async () => {
    const dir = await fs.mkdtemp(path.join(os.tmpdir(), "test-"));
    const filename = `${dir}/test.txt`;
    await createFile(filename).run();
    const stat = await fs.stat(filename);

    expect(stat.isFile()).toBeTruthy();
    expect(stat.size).toBe(0);
  });

  it("cannot create a file in an illegal directory", async () => {
    const filename = "/usr/local/bin/bruh";
    const res = await createFile(filename).run();

    expect(res.isLeft()).toBeTruthy();
  });
});
