/** @type {import('eslint').Linter.Config} */
module.exports = {
  root: true,
  env: {
    browser: true,
    es2021: true,
    node: true,
  },
  extends: ["@sondr3/eslint-config/typescript"],
  plugins: ["svelte3"],
  ignorePatterns: ["*.cjs", "*.js"],
  parser: "@typescript-eslint/parser",
  overrides: [{ files: ["*.svelte"], processor: "svelte3/svelte3" }],
  settings: {
    "svelte3/typescript": () => require("typescript"),
  },
  parserOptions: {
    project: "tsconfig.json",
    ecmaVersion: 12,
    sourceType: "module",
  },
};
