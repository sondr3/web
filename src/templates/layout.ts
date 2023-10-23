import { Content } from "../content.ts";
import { html } from "./html.ts";

export interface Layout {
  content: Content;
  render(): string;
}

abstract class BaseLayout implements Layout {
  content: Content;

  constructor(content: Content) {
    this.content = content;
  }

  abstract render(): string;
}

export class PageLayout extends BaseLayout {
  render(): string {
    const page = this.content.content;

    return html`
<article class="prose">
  ${page}
</article>
`.trim();
  }
}

export class FourOhFourLayout extends BaseLayout {
  render(): string {
    return html`
<section class="four-oh-four">
  <h1>Page not found</h1>
  <p class="prose">What you're looking for does not exist :(</p>
  <div>
    <a href="/" class="blue">
      Go back home<span aria-hidden="true">&rarr;</span>
    </a>
  </div>
</section>
    `.trim();
  }
}

export class IndexLayout extends BaseLayout {
  render(): string {
    return html`
<section>
  <h1 id="hello">
    <span>Hello! I'm</span>
    <span class="blue">Sondre</span>
  </h1>
  <p class="me prose">
    I am a full time nerd with a passion for programming languages, mechanical keyboards, hoarding side-projects
    and occasionally creating useful software.
  </p>
</section>
        `.trim();
  }
}
