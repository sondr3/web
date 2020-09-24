import { dirWalk } from "./fs";

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
