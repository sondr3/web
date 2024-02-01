import djot from "@djot/djot";
import type { HTMLRenderOptions } from "@djot/djot/types/html";
import { getHighlighter } from "shiki";

const shiki = await getHighlighter({
	themes: ["nord"],
	langs: ["javascript", "typescript", "toml", "yaml", "css", "html", "rust", "sql", "json"],
});

export const render_djot = (content: string) => {
	const parsed = djot.parse(content, { warn: (message) => console.warn(message) });
	return djot.renderHTML(parsed, renderOptions);
};

const renderOptions: HTMLRenderOptions = {
	overrides: {
		section: (node, renderer) => {
			if (node?.attributes?.["id"]) {
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
