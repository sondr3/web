import { type ELEMENT_MAP, h } from "@sondr3/radiant";

export const page = (content: string): ELEMENT_MAP["article"] => h.article({ class: "prose" }, h.raw(content));
