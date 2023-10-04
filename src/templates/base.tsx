import type { JSX } from "preact";
import { type Frontmatter } from "../content.ts";
import { Footer } from "./footer.tsx";
import { Navbar } from "./navbar.tsx";
import { Asset } from "../asset.ts";

const THEME_LOCALSTORAGE_SCRIPT = `
if (localStorage.theme === "dark" || (!("theme" in localStorage) && window.matchMedia("(prefers-color-scheme: dark)").matches)) {
  document.documentElement.setAttribute("data-theme", "dark");
}
`.trim();

const THEME_SWITCH_SCRIPT = `
function toggle() {
  if (document.documentElement.getAttribute("data-theme") === "dark") {
    document.documentElement.setAttribute("data-theme", "light");
    window.localStorage.setItem("theme", "light");
  } else {
    document.documentElement.setAttribute("data-theme", "dark");
    window.localStorage.setItem("theme", "dark");
  }
}

document
  .querySelectorAll(".theme-toggle")
  .forEach((e) => e.addEventListener("click", toggle));
`.trim();

export interface BaseProps {
  assets: Map<string, Asset>;
}

export interface Props extends BaseProps {
  fm: Frontmatter;
  children: JSX.Element;
}

export const Base = ({ fm, children, assets }: Props) => {
  const title = `${fm.title} => Eons :: IO ()`;
  const css = assets.get("styles.css");

  return (
    <html lang="en" data-theme="light">
      <head>
        <meta charSet="UTF-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />

        <link rel="icon" type="image/svg+xml" href="/favicon.svg" />
        <link href="/favicon.ico" rel="icon" />
        <link
          rel="alternate icon"
          href="/icon-192.png"
          sizes="192x192"
          type="image/png"
        />
        <link
          rel="alternate icon"
          href="/icon-512.png"
          sizes="512x512"
          type="image/png"
        />
        <link href="/apple-touch-icon.png" rel="apple-touch-icon" sizes="180x180" />

        <link href={`/${css?.filename}`} rel="stylesheet" />

        <link href="/humans.txt" rel="author" />
        <meta content="Sondre Aasemoen" name="author" />
        <link rel="me" href="https://fosstodon.org/@sondre" />

        <link
          as="font"
          crossOrigin="anonymous"
          href="/fonts/Piazzolla.woff2"
          rel="preload"
          type="font/woff2"
        />
        <link
          as="font"
          crossOrigin="anonymous"
          href="/fonts/Inconsolata.woff2"
          rel="preload"
          type="font/woff2"
        />

        <title>{title}</title>
        <meta content={fm.description} name="description" />
        <link href="{{ canonical_url }}" rel="canonical" />

        <meta content="en" property="og:locale" />
        <meta content="website" property="og:type" />
        <meta content={title} property="og:title" />
        <meta content={fm.description} property="og:description" />
        <meta content="{{ canonical_url }}" property="og:url" />
        {/* <!-- <meta property="og:image" /> --> */}
        {/* <!-- <meta content="2020-12-12" property="article:published_time" /> --> */}
        {/* <!-- <meta content="2022-01-10" property="article:modified_time" /> --> */}
        {/* <!-- <meta content="summary_large_image" name="twitter:card" /> --> */}

        <meta content="sondr3" name="twitter:site" />

        <script src="/livereload.js"></script>
        <script dangerouslySetInnerHTML={{ __html: THEME_LOCALSTORAGE_SCRIPT }} />
      </head>
      <body class="root">
        <Navbar />
        {children}
        <Footer />
        <script dangerouslySetInnerHTML={{ __html: THEME_SWITCH_SCRIPT }} />
      </body>
    </html>
  );
};
