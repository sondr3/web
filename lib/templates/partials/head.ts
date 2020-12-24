import { html } from "../"
import { getStyle } from "../../state"

export const head = (title: string): string => html`
  <head>
    <meta charset="UTF-8" />

    <title>${title}</title>
    <meta name="description" content="{{ description }}" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />

    <link rel="author" href="humans.txt" />

    <link rel="stylesheet" href="${getStyle("style.css")}" />
  </head>
`
