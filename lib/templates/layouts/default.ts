import { html } from ".."
import { footer, head, nav } from "../partials"

export const layout = (title: string, content: string): string => html`
  <!DOCTYPE html>
  <html lang="en">
    ${head(title)}
    <body>
      <div class="container">${nav()} ${content} ${footer()}</div>
    </body>
  </html>
`
