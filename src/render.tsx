import renderToStringPretty from "preact-ssr";
import { Content } from "./content.ts";
import { Page } from "./templates/page.tsx";
import type { JSX } from "preact";
import { FourOhFour } from "./templates/404.tsx";
import { Index } from "./templates/index.tsx";
import { Site } from "./site.ts";

const renderPretty = (elem: JSX.Element) => {
  const hello = renderToStringPretty(elem, {}, { pretty: true });
  return `<!DOCTYPE html>\n${hello}`;
};

export const renderTemplate = (
  content: Content,
  site: Site,
) => {
  switch (content.frontmatter.layout) {
    case "page":
      return renderPretty(<Page content={content} site={site} />);
    case "404":
      return renderPretty(<FourOhFour content={content} site={site} />);
    case "index":
      return renderPretty(<Index content={content} site={site} />);
  }
};
