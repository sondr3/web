import minify from "@minify-html/js";
import { createRequire } from "node:module";
import path from "node:path";

import { Config } from "../build/config.js";
import { Content } from "../build/content.js";
import { Context } from "../context.js";
import { walkDir } from "../utils/fs.js";
import { base } from "./base.js";

const require = createRequire(import.meta.url);
const mf = require("@minify-html/js") as typeof minify;

type TemplateFn = (content: Content) => string;

export class Template {
  templates: Map<string, TemplateFn> = new Map();

  update = async (config: Config): Promise<void> => {
    this.templates.clear();
    for await (const file of walkDir(config.templates, (file) => file.endsWith(".mjs"))) {
      const fn: unknown = await import(`${file}?fml=${performance.now().toFixed(0)}`);
      const template = (fn as { default: TemplateFn }).default;
      this.templates.set(path.parse(file).name, template);
    }
  };

  render = (content: Content, { site, config }: Context): Buffer => {
    const layout = this.templates.get(content.metadata.layout);
    if (!layout) throw new Error(`Template ${content.metadata.layout} not found`);

    return this.minifyHtml(
      base(Content.fromLayout(content, layout(content)), site, config),
      config.production,
    );
  };

  minifyHtml = (html: string, production: boolean): Buffer => {
    if (!production) return Buffer.from(html);
    return mf.minify(html, mf.createConfiguration({ minify_js: true, minify_css: false }));
  };
}
