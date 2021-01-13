import { Site } from "../../site"
import { html } from ".."
import { footer, head, nav } from "../partials"

export const layout = (site: Site, title: string, content: string): string => html`
  <!DOCTYPE html>
  <html lang="en">
    ${head(site, title)}
    <body>
      <div class="container">${nav} ${content} ${footer}</div>
    </body>
  </html>
`
