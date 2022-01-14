import { html } from "../dist/templates/templating.js";

export default (_content) => html`<section>
  <h1 class="hello">Not found :(</h1>
  <p>You attempted to visit a page that does not exist. Whoops.</p>
</section>`;
