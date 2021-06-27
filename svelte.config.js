import vercel from "@sveltejs/adapter-vercel"
import { mdsvex } from "mdsvex"
import path from "path"
import preprocess from "svelte-preprocess"

const isDev = process.env.NODE_ENV === "development"

/** @type {import('@sveltejs/kit').Config} */
const config = {
  extensions: [".svelte", ".md", ".svx"],
  preprocess: [
    mdsvex({
      extensions: [".md", ".svx"],
      layout: {
        _: "./src/components/layout/Default.svelte",
      },
    }),
    preprocess({
      scss: {
        sourceMap: !isDev,
        prependData: "@use 'src/styles/_variables.scss' as *;",
      },
    }),
  ],
  kit: {
    target: "#svelte",
    adapter: vercel(),
    vite: {
      resolve: {
        alias: {
          $components: path.resolve("./src/components"),
          $styles: path.resolve("./src/styles"),
        },
      },
    },
  },
}

export default config
