import prefetch from "@astrojs/prefetch";
import sitemap from "@astrojs/sitemap";
import { defineConfig } from "astro/config";
import compressor from "astro-compressor";
import htmlMinifier from "astro-html-minifier";
import tailwind from "@astrojs/tailwind";
import mdx from "@astrojs/mdx";

export default defineConfig({
  site: "https://www.eons.io",
  trailingSlash: "always",
  compressHTML: false,
  integrations: [
    mdx(),
    tailwind({
      applyBaseStyles: false,
    }),
    prefetch(),
    sitemap(),
    htmlMinifier(),
    compressor(),
  ],
});
