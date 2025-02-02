import { defineConfig } from "astro/config";

import compressor from "astro-compressor";

export default defineConfig({
  base: "/",
  site: "https://www.eons.io",
  trailingSlash: "always",
  output: "static",
  build: {
    format: "directory",
  },
  devToolbar: {
    enabled: false,
  },
  integrations: [compressor()],
});
