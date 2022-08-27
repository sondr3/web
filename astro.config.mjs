import { defineConfig } from "astro/config";
import tailwind from "@astrojs/tailwind";
import sitemap from "@astrojs/sitemap";
import solidJs from "@astrojs/solid-js";
import htmlMinifier from "astro-html-minifier";
import compressor from "astro-compressor";

// https://astro.build/config
export default defineConfig({
  site: "https://www.eons.io",
  trailingSlash: "always",
  integrations: [solidJs(), tailwind(), sitemap(), htmlMinifier(), compressor()],
});
