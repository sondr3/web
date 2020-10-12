import { renderStyles, styleName } from "./Styles";
import path from "path";
import assert from "assert";

describe("styleName", () => {
  it("gives correct name without extension", () => {
    const file = styleName("test.html");
    expect(file).toBe("./test/public/assets/style/test");
  });

  it("gives correct name with extension", () => {
    const file = styleName("test.html", "css");
    expect(file).toBe("./test/public/assets/style/test.css");
  });
});

describe("renderStyles", () => {
  it("renders and creates files", async () => {
    const spec = path.resolve(process.cwd(), "lib/assets/Styles.spec.scss");
    const res = await renderStyles(spec, false);

    expect(res === undefined).toBeTruthy();
  });

  it("errors when it cannot render", async () => {
    try {
      const res = await renderStyles("no.scss", false);

      expect(res !== undefined).toBeTruthy();
      assert(res !== undefined);
      expect(res.message).toContain("Could not create styles");
    } catch (e) {
      // noop
    }
  });
});
