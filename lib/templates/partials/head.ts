import { getStyle } from "../../site/state"
import { html } from ".."

export const head = (title: string): string => html`
  <head>
    <meta charset="UTF-8" />

    <title>${title}</title>
    <meta name="description" content="{{ description }}" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />

    <link rel="author" href="humans.txt" />

    <link rel="manifest" href="/manifest.webmanifest" />
    <link rel="icon" href="/favicon.ico" />
    <link rel="icon" href="/icon.svg" type="image/svg+xml" sizes="any" />
    <link rel="apple-touch-icon" href="/apple-touch-icon.png" />

    <link rel="stylesheet" href="${getStyle("style.css")}" />
  </head>
`
