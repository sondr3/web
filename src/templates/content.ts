import { h } from "@sondr3/radiant";

export const page = (content: string) => h.article({ class: "prose" }, h.raw(content));
