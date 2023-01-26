import mdx from "@astrojs/mdx";
import prefetch from "@astrojs/prefetch";
import sitemap from "@astrojs/sitemap";
import tailwind from "@astrojs/tailwind";
import { defineConfig } from "astro/config";
import compressor from "astro-compressor";
import htmlMinifier from "astro-html-minifier";

// https://astro.build/config
export default defineConfig({
  site: "https://www.eons.io",
  trailingSlash: "always",
  integrations: [
    mdx(),
    prefetch(),
    tailwind({
      config: {
        applyBaseStyles: false,
      },
    }),
    sitemap(),
    htmlMinifier(),
    compressor(),
  ],
});
