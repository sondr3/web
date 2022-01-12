import { createConfiguration, minify } from "@minify-html/js";

import { Content } from "../build/content.js";
import { Site } from "../build/site.js";
import { layout } from "./layout.js";
import { page } from "./page.js";

const minifyHtml = (html: string, production: boolean): Buffer => {
  if (!production) return Buffer.from(html);
  return minify(html, createConfiguration({ minify_js: false, minify_css: false }));
};

/**
 * Render special pages, i.e. not automatically rendered.
 *
 * @param site - Site state
 * @param content - Content to print
 */
export const renderSpecial = (site: Site, content: Content): Buffer => {
  return minifyHtml(layout(content.title(), content.content(), site.style), site.config.production);
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
      res = layout(content.title(), page(content.frontmatter.title, content.content()), site.style);
      break;
    case "post":
      res = layout(content.title(), content.content(), site.style);
      break;
  }

  return minifyHtml(res, site.config.production);
};
