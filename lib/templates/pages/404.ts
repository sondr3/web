import { Site } from "../../site"
import { html } from ".."
import { layout } from "../layouts"

const fourOhFour = html`
  <h1>404: Page not found</h1>
  <p>Oh no :(</p>
`

export const notFound = (site: Site): string => layout(site, "404", fourOhFour)
