import { Config } from "../build/config.js";
import { Content } from "../build/content.js";
import { Site } from "../build/site.js";
import { html } from "./html.js";

export const base = (content: Content, site: Site, config: Config): string => html`
  <!DOCTYPE html>
  <html lang="en" data-theme="light">
    <head>
      <meta charset="utf-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no" />
      <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />

      <link rel="preload" as="style" href="/${site.style}" />
      <link rel="preload" as="script" href="/js/${site.js.get("theme.js")}" />
      <link
        rel="preload"
        as="font"
        type="font/woff2"
        href="/fonts/Piazzolla.woff2"
        crossorigin="anonymous"
      />
      <link
        rel="preload"
        as="font"
        type="font/woff2"
        href="/fonts/Inconsolata.woff2"
        crossorigin="anonymous"
      />

      <link rel="icon" href="/favicon.ico" />
      <link rel="icon" href="/icon.svg" sizes="any" type="image/svg+xml" />
      <link rel="alternate icon" type="image/png" href="/icon-192.png" sizes="192x192" />
      <link rel="alternate icon" type="image/png" href="/icon-512.png" sizes="512x512" />
      <link href="/apple-touch-icon.png" rel="apple-touch-icon" sizes="180x180" />

      <link rel="author" href="/humans.txt" />
      <link rel="stylesheet" href="/${site.style}" />
      <link rel="canonical" href="${config.url}${content.url()}" />

      <meta name="author" content="Sondre Nilsen" />
      <meta name="description" content="${content.frontmatter.description}" />
      <title>${content.title()}</title>

      <meta property="og:locale" content="en" />
      <meta property="og:type" content="${content.type()}" />
      <meta property="og:site_name" content="Eons :: IO ()" />
      <meta property="og:title" content="${content.title()}" />
      <meta property="og:description" content="${content.frontmatter.description}" />
      <meta property="og:url" content="${config.url}${content.url()}" />
      <meta property="og:image" content="" />
      ${content.isArticle() && [
        `<meta property="article:published_time" content="${content.createdDate()}" />`,
        `<meta property="article:modified_time" content="${content.modifiedDate()}" />`,
      ]}

      <meta name="twitter:card" content="summary_large_image" />
      <meta name="twitter:site" content="sondr3" />
      <script>
        const set = (val) => document.documentElement.setAttribute("data-theme", val);
        if (window.localStorage.getItem("theme")) {
          set(window.localStorage.getItem("theme"));
        } else if (window.matchMedia("(prefers-color-scheme: dark)").matches) {
          set("dark");
        }
        window
          .matchMedia("(prefers-color-scheme: dark)")
          .addEventListener("change", (e) => set(e.matches ? "dark" : "light"));
      </script>
    </head>
    <body class="root">
      <header class="header">
        <h1 class="title">
          <a href="/">EONS :: IO ()</a>
        </h1>
        <nav class="nav">
          <ul class="nav__links">
            <!-- <li><a href="/">projects</a></li> -->
            <!-- <li><a href="/">articles</a></li> -->
            <!-- <li class="nav__link"><a href="/resume/">resume</a></li> -->
            <li class="nav__link"><a href="/about/">about</a></li>
          </ul>
        </nav>
        <button class="theme-btn -light" aria-hidden="true" tabindex="-1">
          <svg
            xmlns="http://www.w3.org/2000/svg"
            class=""
            fill="none"
            viewBox="0 0 24 24"
            stroke="currentColor"
            width="24"
          >
            <path
              stroke-linecap="round"
              stroke-linejoin="round"
              stroke-width="2"
              d="M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z"
            />
          </svg>
        </button>
        <button class="theme-btn -dark" aria-hidden="true" tabindex="-1">
          <svg
            xmlns="http://www.w3.org/2000/svg"
            class=""
            fill="none"
            viewBox="0 0 24 24"
            stroke="currentColor"
            width="24"
          >
            <path
              stroke-linecap="round"
              stroke-linejoin="round"
              stroke-width="2"
              d="M20.354 15.354A9 9 0 018.646 3.646 9.003 9.003 0 0012 21a9.003 9.003 0 008.354-5.646z"
            />
          </svg>
        </button>
      </header>
      <main class="main">${content.content()}</main>
      <footer class="footer">
        <p>
          Content (C) Sondre Nilsen, licensed under
          <a href="https://creativecommons.org/licenses/by-sa/4.0/">CC BY-SA 4.0</a>
        </p>
      </footer>
    </body>
    ${!config.production && "<script type='text/javascript' src='/js/livereload.js'></script>"}
    <script type="text/javascript" src="/js/${site.js.get("theme.js")}"></script>
  </html>
`;
