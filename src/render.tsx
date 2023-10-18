import renderToStringPretty from "preact-ssr";
import { Content } from "./content.ts";
import { Page } from "./templates/page.tsx";
import type { JSX } from "preact";
import { FourOhFour } from "./templates/404.tsx";
import { Index } from "./templates/index.tsx";
import { Context } from "./context.ts";

const renderPretty = (elem: JSX.Element) => {
  const hello = renderToStringPretty(elem, {}, { pretty: true });
  return `<!DOCTYPE html>\n${hello}`;
};

export const renderTemplate = (
  content: Content,
  context: Context,
) => {
  switch (content.frontmatter.layout) {
    case "page":
      return renderPretty(<Page content={content} context={context} />);
    case "404":
      return renderPretty(<FourOhFour content={content} context={context} />);
    case "index":
      return renderPretty(<Index content={content} context={context} />);
  }
};
