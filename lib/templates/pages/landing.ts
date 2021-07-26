import { defaultConfig, Site } from "../../site"
import { html } from ".."
import { layout } from "../layouts"

const {
  meta: { author },
} = defaultConfig

const landingPage = html`
  <section class="intro">
    <div class="about">
      <div class="hello">
        <h1>Hello!</h1>
        <h2>I&apos;m Sondre.</h2>
      </div>
      <div>
        <img class="portrait" src="/images/developer.svg" alt="${author.intro}" />
      </div>
    </div>
    <section>
      <p>${author.intro}. You can see them on my <a href="${author.socials.github}">GitHub</a>.</p>
    </section>
  </section>
`

export const landing = (site: Site): string => {
  return layout(site, "EONS", landingPage)
}
