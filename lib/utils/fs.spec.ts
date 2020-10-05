import { createDirectory, dirWalk } from "./fs";

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
