import minify from "@minify-html/js";
import { createRequire } from "node:module";
import path from "node:path";
const require = createRequire(import.meta.url);
const mf = require("@minify-html/js") as typeof minify;

import { Config } from "../build/config.js";
import { Content } from "../build/content.js";
import { Context } from "../context.js";
import { walkDir } from "../utils/fs.js";
import { base } from "./base.js";
import { page } from "./page.js";

type Template = (content: Content) => string;

export class Templating {
  templates: Map<string, Template> = new Map();

  init = async (config: Config): Promise<void> => {
    for await (const file of walkDir(config.templates, (file) => file.endsWith(".mjs"))) {
      const imp = (await import(file)) as { default: Template };
      const template = imp.default;
      this.templates.set(path.parse(file).name, template);
    }
  };

  render = (content: Content, { site, config }: Context): Buffer => {
    const layout = this.templates.get(content.metadata.layout);
    if (!layout) throw new Error("what");

    return this.minifyHtml(
      base(Content.fromLayout(content, layout(content)), site, config),
      config.production,
    );
  };

  minifyHtml = (html: string, production: boolean): Buffer => {
    if (!production) return Buffer.from(html);
    return mf.minify(html, mf.createConfiguration({ minify_js: false, minify_css: false }));
  };
}

const minifyHtml = (html: string, production: boolean): Buffer => {
  if (!production) return Buffer.from(html);
  return mf.minify(html, mf.createConfiguration({ minify_js: false, minify_css: false }));
};

/**
 * Render special pages, i.e. not automatically rendered.
 *
 * @param site - Site state
 * @param config - Configuration
 * @param content - Content to print
 */
export const renderSpecial = ({ site, config }: Context, content: Content): Buffer => {
  return minifyHtml(base(content, site, config), config.production);
};

/**
 * Render all other pages
 *
 * @param site - Site state
 * @param config - Configuration
 * @param content - Content to print
 */
export const renderLayout = ({ site, config }: Context, content: Content): Buffer => {
  let res: string;
  switch (content.metadata.layout) {
    case "page":
      res = base(
        new Content(
          content.metadata,
          content.frontmatter,
          page(content.frontmatter.title, content.content()),
        ),
        site,
        config,
      );
      break;
    case "post":
      res = base(
        new Content(
          content.metadata,
          content.frontmatter,
          page(content.frontmatter.title, content.content()),
        ),
        site,
        config,
      );
      break;
  }

  return minifyHtml(res, config.production);
};
