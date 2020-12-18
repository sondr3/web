import { getConfig } from "../../config"
import { html } from "../../build/html"
import { layout } from "../layouts"

const {
  meta: { author },
} = getConfig()

const landingPage = html`
  <section class="intro">
    <div class="about">
      <div class="hello">
        <h1>Hello!</h1>
        <h2>I&apos;m Sondre.</h2>
      </div>
      <div>
        <img class="portrait" src="/static/developer.svg" alt="${author.intro}" />
      </div>
    </div>
    <section>
      <p>${author.intro}. You can see them on my <a href="${author.socials.github}">GitHub</a>.</p>
    </section>
  </section>
`

export const landing = (): string => {
  return layout("EONS", landingPage)
}
