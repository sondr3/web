import { Config } from "../build/config.js";
import { Content } from "../build/content.js";
import { Site } from "../build/site.js";
import { html } from "./html.js";

export const base = (content: Content, site: Site, config: Config): string => html`
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="utf-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no" />
      <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />

      <link rel="preload" as="font" type="font/woff2" href="/fonts/Piazzolla.woff2" crossorigin />
      <link
        rel="preload"
        as="font"
        type="font/woff2"
        href="/fonts/PiazzollaItalic.woff2"
        crossorigin
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
    </head>
    <body class="root">
      <header class="header">
        <h1 class="title">
          <a href="/">EONS :: IO ()</a>
        </h1>
        <nav>
          <ul class="nav">
            <!-- <li><a href="/">projects</a></li> -->
            <!-- <li><a href="/">articles</a></li> -->
            <!-- <li class="link"><a href="/resume/">resume</a></li> -->
            <li class="link"><a href="/about/">about</a></li>
          </ul>
        </nav>
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
  </html>
`;
