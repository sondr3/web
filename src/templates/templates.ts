import minify from "@minify-html/js";
import { createRequire } from "node:module";
const require = createRequire(import.meta.url);
const mf = require("@minify-html/js") as typeof minify;

import { Content } from "../build/content.js";
import { Site } from "../build/site.js";
import { layout } from "./layout.js";
import { page } from "./page.js";

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
  return minifyHtml(layout(content, site), site.config.production);
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
      res = layout(
        new Content(
          content.metadata,
          content.frontmatter,
          page(content.frontmatter.title, content.content()),
        ),
        site,
      );
      break;
    case "post":
      res = layout(
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
