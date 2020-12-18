import { html } from "../"

export const section = (titleId: string, title: string, content: string): string => html`
  <section>
    <h2 id="${titleId}">${title}</h2>
    ${content}
  </section>
`
