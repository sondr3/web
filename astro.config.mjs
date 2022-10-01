import { defineConfig } from "astro/config";
import tailwind from "@astrojs/tailwind";
import sitemap from "@astrojs/sitemap";
import htmlMinifier from "astro-html-minifier";
import compressor from "astro-compressor";

// https://astro.build/config
export default defineConfig({
  site: "https://www.eons.io",
  trailingSlash: "always",
  integrations: [
    tailwind({
      config: { applyBaseStyles: false },
    }),
    sitemap(),
    htmlMinifier(),
    compressor(),
  ],
});
