/** @type {import('prettier').Options} */
module.exports = {
  ...require("@sondr3/prettier"),
  plugins: ["./node_modules/prettier-plugin-svelte"],
  svelteSortOrder: "scripts-options-markup-styles",
  svelteStrictMode: true,
  svelteAllowShorthand: true,
  overrides: [
    {
      files: "*.svelte",
      options: { parser: "svelte" },
    },
  ],
};
