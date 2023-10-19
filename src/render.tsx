import renderToStringPretty from "preact-pretty";
import { render } from "preact-render";
import { Content } from "./content.ts";
import { Page } from "./templates/page.tsx";
import type { JSX } from "preact";
import { FourOhFour } from "./templates/404.tsx";
import { Index } from "./templates/index.tsx";
import { Site } from "./site.ts";

const renderPretty = (elem: JSX.Element, site: Site) => {
  const rendered = site.isProd() ? render(elem) : renderToStringPretty(elem, {}, { pretty: true });
  return `<!DOCTYPE html>\n${rendered}`;
};

export const renderTemplate = (
  content: Content,
  site: Site,
) => {
  switch (content.frontmatter.layout) {
    case "page":
      return renderPretty(<Page content={content} site={site} />, site);
    case "404":
      return renderPretty(<FourOhFour content={content} site={site} />, site);
    case "index":
      return renderPretty(<Index content={content} site={site} />, site);
  }
};
