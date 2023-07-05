const { fontFamily } = require("tailwindcss/defaultTheme");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./templates/**/*.{html}"],
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
