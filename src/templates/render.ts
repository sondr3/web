import { Content } from "../content.ts";
import { FourOhFourLayout, IndexLayout, Layout, PageLayout } from "./layout.ts";

export const renderTemplate = (content: Content): Layout => {
  switch (content.frontmatter.layout) {
    case "page":
      return new PageLayout(content);
    case "404":
      return new FourOhFourLayout(content);
    case "index":
      return new IndexLayout(content);
  }
};
