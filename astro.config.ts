import { defineConfig } from "astro/config";
import compressor from "astro-compressor";

export default defineConfig({
  srcDir: "./src",
  base: "/",
  site: "https://www.eons.io",
  trailingSlash: "always",
  output: "static",
  build: {
    format: "directory",
    inlineStylesheets: "never",
  },
  compressHTML: true,
  integrations: [compressor()],
});
