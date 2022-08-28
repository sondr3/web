module.exports = {
  ...require("@sondr3/prettier"),
  plugins: [require.resolve("prettier-plugin-astro"), require("prettier-plugin-tailwindcss")],
  overrides: [
    {
      files: "*.astro",
      options: {
        parser: "astro",
      },
    },
  ],
};
