import { html } from "../../html";
import { head, nav, footer } from "../partials";

export const layout = (title: string, content: string): string => html`
  <!DOCTYPE html>
  <html lang="en">
    ${head(title)}
    <body>
      <div class="container">${nav()} ${content} ${footer()}</div>
    </body>
  </html>
`;
