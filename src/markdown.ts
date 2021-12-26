import MarkdownIt from "https://esm.sh/markdown-it@12.3.0";

export class Markdown {
  private md: { render(input: string): string };

  constructor() {
    this.md = new MarkdownIt({
      html: true,
      typographer: true,
    });
  }

  render(input: string): string {
    return this.md.render(input);
  }
}
