import { html } from "../dist/templates/html.js";

export default (_content) => html`<section>
    <h1 class="hello">
      <span>Hello! I&apos;m</span>
      <span class="blue"> Sondre</span>
    </h1>
    <p class="me">
      I am a full time nerd with a passion for programming languages, mechanical keyboards, hoarding
      side-projects and occasionally creating useful software.
    </p>
  </section>
  `;
