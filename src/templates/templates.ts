import { Content } from "../build/content.js";
import { Site } from "../build/site.js";
import { layout } from "./layout.js";
import { page } from "./page.js";

/**
 * Render special pages, i.e. not automatically rendered.
 *
 * @param site - Site state
 * @param content - Content to print
 */
export const renderSpecial = (site: Site, content: Content): string => {
  return layout(content.title(), content.content(), site.style);
};

/**
 * Render all other pages
 *
 * @param site - Site state
 * @param content - Content to print
 */
export const renderLayout = (site: Site, content: Content): string => {
  switch (content.metadata.layout) {
    case "page":
      return layout(
        content.title(),
        page(content.frontmatter.title, content.content()),
        site.style,
      );
    case "post":
      return layout(content.title(), content.content(), site.style);
  }
};
