import renderToStringPretty from "preact-ssr";
import { Frontmatter } from "./content.ts";
import { Page } from "./templates/page.tsx";
import type { JSX } from "preact";
import { FourOhFour } from "./templates/404.tsx";
import { Index } from "./templates/index.tsx";
import { Asset } from "./asset.ts";

const renderPretty = (elem: JSX.Element) => {
  const hello = renderToStringPretty(elem, {}, { pretty: true });
  return `<!DOCTYPE html>\n${hello}`;
};

export const renderTemplate = (fm: Frontmatter, content: string, assets: Map<string, Asset>) => {
  switch (fm.layout) {
    case "page":
      return renderPretty(<Page fm={fm} content={content} assets={assets} />);
    case "404":
      return renderPretty(<FourOhFour fm={fm} assets={assets} />);
    case "index":
      return renderPretty(<Index fm={fm} assets={assets} />);
  }
};