import mdx from "@astrojs/mdx";
import prefetch from "@astrojs/prefetch";
import sitemap from "@astrojs/sitemap";
import { defineConfig } from "astro/config";
import compressor from "astro-compressor";
import htmlMinifier from "astro-html-minifier";

export default defineConfig({
  site: "https://www.eons.io",
  trailingSlash: "always",
  integrations: [mdx(), prefetch(), sitemap(), htmlMinifier(), compressor()],
  vite: {
    css: {
      devSourcemap: true,
      preprocessorOptions: {
        scss: { additionalData: `@use "src/styles/variables" as v;` },
      },
    },
  },
});
