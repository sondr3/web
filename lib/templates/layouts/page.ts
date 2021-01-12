import { Site } from "../../site"
import { html } from ".."
import { layout } from "./default"

const _page = (title: string, content: string) => html`
  <h1>${title}</h1>
  ${content}
`

export const page = (site: Site, title: string, content: string): string => {
  return layout(site, title, _page(title, content))
}
