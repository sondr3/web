import { slugify } from "../dist/utils/utils.js";
import { html } from "../dist/templates/templating.js";

export default (content) => {
  return html`
    <h1 id="${slugify(content.frontmatter.title)}">${content.frontmatter.title}</h1>
    ${content.content()}
  `;
};
