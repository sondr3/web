module.exports = {
  env: {
    browser: true,
    es2017: true,
    node: true,
  },
  extends: ["@sondr3/eslint-config/typescript", "@sondr3/eslint-config/node"],
  plugins: ["svelte3"],
  parser: "@typescript-eslint/parser",
  parserOptions: {
    project: "tsconfig.json",
    ecmaVersion: 12,
    sourceType: "module",
  },
  ignorePatterns: ["*.cjs", "*.js"],
  overrides: [{ files: ["*.svelte"], processor: "svelte3/svelte3" }],
  settings: {
    "svelte3/typescript": () => require("typescript"),
    "svelte3/ignore-styles": () => true,
  },
  rules: {
    "node/no-unpublished-import": "off",
  },
  overrides: [],
}
