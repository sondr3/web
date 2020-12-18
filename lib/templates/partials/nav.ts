import { html } from "../"

export const nav = (): string => html`
  <header>
    <a href="/" class="title">Eons</a>
    <nav>
      <a href="/about/" class="nav-link">About</a>
    </nav>
  </header>
`
