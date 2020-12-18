import { html } from "../../build/html"

export const preamble = (content: string): string => html`<section class="preamble">${content}</section>`
