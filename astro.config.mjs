import { defineConfig } from "astro/config";
import tailwind from "@astrojs/tailwind";
import sitemap from "@astrojs/sitemap";
import htmlMinifier from "astro-html-minifier";
import compressor from "astro-compressor";
import prefetch from "@astrojs/prefetch";
import mdx from "@astrojs/mdx";

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
