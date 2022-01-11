import { html } from "../templating.js";
import { layout } from "./layout.js";

const content = html`<section>
  <h1>
    <span>Hello! I&apos;m</span>
    <span class="blue"> Sondre</span>
  </h1>
  <p>
    I am a full time nerd with a passion for programming languages, mechanical keyboards, hoarding
    sideprojects and occasionally creating useful software.
  </p>
</section>`;

export const landing = (): string => layout("home", content);
