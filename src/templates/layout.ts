import { html } from "../templating.js";

export const layout = (title: string, content: string): string => html`
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="utf-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no" />
      <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />

      <link rel="icon" href="/favicon.ico" />
      <link rel="icon" href="/icon.svg" sizes="any" type="image/svg+xml" />
      <link rel="alternate icon" type="image/png" href="/icon-192.png" sizes="192x192" />
      <link rel="alternate icon" type="image/png" href="/icon-512.png" sizes="512x512" />
      <link href="/apple-touch-icon.png" rel="apple-touch-icon" sizes="180x180" />

      <link rel="author" href="/humans.txt" />
      <title>${title}</title>
    </head>
    <body>
      <header>
        <h1>
          <a href="/">EONS :: IO ()</a>
        </h1>
        <nav>
          <ul>
            <!-- <li><a href="/">projects</a></li> -->
            <!-- <li><a href="/">articles</a></li> -->
            <li><a href="/about/">about</a></li>
          </ul>
        </nav>
      </header>
      <main>${content}</main>
      <footer>
        <p>
          Content (C) Sondre Nilsen, licensed under
          <a href="https://creativecommons.org/licenses/by-sa/4.0/">CC BY-SA 4.0</a>
        </p>
      </footer>
    </body>
  </html>
`;
