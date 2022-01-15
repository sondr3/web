import minify from "@minify-html/js";
import { createRequire } from "node:module";
import path from "node:path";
const require = createRequire(import.meta.url);
const mf = require("@minify-html/js") as typeof minify;

import { Content } from "../build/content.js";
import { Site } from "../build/site.js";
import { walkDir } from "../utils/fs.js";
import { base } from "./base.js";
import { page } from "./page.js";

type Template = (content: Content) => string;

export class Templating {
  templates: Map<string, Template> = new Map();

  init = async (site: Site): Promise<void> => {
    for await (const file of walkDir(site.config.templates, (file) => file.endsWith(".mjs"))) {
      const imp = (await import(file)) as { default: Template };
      const template = imp.default;
      this.templates.set(path.parse(file).name, template);
    }
  };

  render = (content: Content, site: Site): Buffer => {
    const layout = this.templates.get(content.metadata.layout);
    if (!layout) throw new Error("what");

    return this.minifyHtml(
      base(Content.fromLayout(content, layout(content)), site),
      site.config.production,
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
 * @param content - Content to print
 */
export const renderSpecial = (site: Site, content: Content): Buffer => {
  return minifyHtml(base(content, site), site.config.production);
};

/**
 * Render all other pages
 *
 * @param site - Site state
 * @param content - Content to print
 */
export const renderLayout = (site: Site, content: Content): Buffer => {
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
      );
      break;
  }

  return minifyHtml(res, site.config.production);
};
