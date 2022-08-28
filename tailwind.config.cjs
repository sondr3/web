const { fontFamily } = require("tailwindcss/defaultTheme");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{astro,html,js,jsx,md,mdx,svelte,ts,tsx,vue}"],
  darkMode: "class",
  theme: {
    fontFamily: {
      sans: ["Piazzolla", ...fontFamily.sans],
      serif: ["Piazzolla", ...fontFamily.serif],
      mono: ["Inconsolata", ...fontFamily.mono],
    },
    extend: {
      typography: {
        DEFAULT: {
          css: {
            p: {
              "text-align": "justify",
              "text-justify": "inter-word",
            },
          },
        },
      },
    },
  },
  plugins: [require("@tailwindcss/typography")],
};
