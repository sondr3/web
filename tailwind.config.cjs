const { fontFamily } = require("tailwindcss/defaultTheme");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{astro,html,js,jsx,md,mdx,svelte,ts,tsx,vue}"],
  theme: {
    fontFamily: {
      sans: ["Piazzolla", ...fontFamily.sans],
      serif: ["Piazzolla", ...fontFamily.serif],
      mono: ["Inconsolata", ...fontFamily.mono],
    },
    extend: {},
  },
  plugins: [require("@tailwindcss/typography")],
};
