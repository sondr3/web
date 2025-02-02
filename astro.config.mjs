import { defineConfig } from "astro/config";

import compressor from "astro-compressor";

export default defineConfig({
  base: "/",
  site: "https://www.eons.io",
  trailingSlash: "ignore",
  output: "static",

  devToolbar: {
    enabled: false,
  },

  integrations: [compressor()],
});
