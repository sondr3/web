import { formatHtml } from "./formatting";

describe("formatHtml", () => {
  it("formats HTML", () => {
    const html = "<div  >Hello<span >world</span></div >";
    expect(formatHtml(html)).toBe("<div>Hello<span>world</span></div>\n");
  });
});
