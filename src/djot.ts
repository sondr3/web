import djot, { HTMLRenderer } from "djot";
import { getHighlighter } from "shikiji";

const shiki = await getHighlighter({
  themes: ["nord"],
  langs: ["javascript", "typescript", "toml", "yaml", "css", "html", "rust", "sql", "json"],
});

export class Djot {
  public static render(content: string) {
    const parsed = djot.parse(content, { warn: (message) => console.warn(message) });
    const res = djot.renderHTML(parsed, this.renderOptions);

    return res;
  }

  private static get renderOptions(): HTMLRenderer["options"] {
    return {
      overrides: {
        section: (node, renderer) => {
          if (node?.attributes?.id) {
            const id = node.attributes["id"].toLowerCase();
            node.attributes = { ...node.attributes, id: id };
          }
          return renderer.renderAstNodeDefault(node);
        },
        code_block: (node, _renderer) => {
          const html = shiki.codeToHtml(node.text.trim(), { lang: node.lang ?? "text", theme: "nord" });
          return html;
        },
      },
    };
  }
}
