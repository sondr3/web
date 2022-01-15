import { slugify } from "../utils/utils.js";
import { html } from "./html.js";

export const page = (title: string, content: string): string => {
  return html`
    <h1 id="${slugify(title)}">${title}</h1>
    ${content}
  `;
};
