import djot, { HTMLRenderer } from "djot";
import { getHighlighter } from "shikiji";

const shiki = await getHighlighter({
  themes: ["nord"],
  langs: ["javascript", "typescript", "toml", "yaml", "css", "html", "rust", "sql", "json"],
});

export class Djot {
  public static render(content: string) {
    const res = djot.renderHTML(
      djot.parse(content, { warn: (message) => console.warn(message) }),
      this.renderOptions,
    );

    return res;
  }

  private static get renderOptions(): HTMLRenderer["options"] {
    return {
      overrides: {
        code_block: (node, _renderer) => {
          const html = shiki.codeToHtml(node.text.trim(), { lang: node.lang ?? "text", theme: "nord" });
          return html;
        },
      },
    };
  }
}
