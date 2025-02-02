import { defineConfig } from "astro/config";

export default defineConfig({
  base: "/",
  site: "https://www.eons.io",
  trailingSlash: "ignore",
  output: "static",
  devToolbar: {
    enabled: false,
  },
});
