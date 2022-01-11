import { layout } from "./layout.js";
import { html } from "./templating.js";

const content = html`<section>
  <h1 class="hello">
    <span>Hello! I&apos;m</span>
    <span class="blue"> Sondre</span>
  </h1>
  <p class="me">
    I am a full time nerd with a passion for programming languages, mechanical keyboards, hoarding
    sideprojects and occasionally creating useful software.
  </p>
</section>`;

export const landing = (): string => layout("Home => Eons :: IO ()", content);
