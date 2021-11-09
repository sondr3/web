/** @type {import('prettier').Options} */
module.exports = {
  ...require("@sondr3/prettier"),
  svelteSortOrder: "scripts-options-markup-styles",
  svelteStrictMode: true,
  svelteBracketNewLine: false,
};
