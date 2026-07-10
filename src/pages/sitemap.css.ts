import type { APIRoute } from "astro";
import css from "../../styles/sitemap.css?inline";

export const GET: APIRoute = () => {
  return new Response(css, {
    headers: { "Content-Type": "text/css" },
  });
};
