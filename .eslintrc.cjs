module.exports = {
  root: true,
  parser: "@typescript-eslint/parser",
  extends: ["@sondr3/eslint-config/typescript", "@sondr3/eslint-config/node"],
  plugins: ["svelte3", "@typescript-eslint"],
  ignorePatterns: ["*.cjs", "*.js"],
  overrides: [{ files: ["*.svelte"], processor: "svelte3/svelte3" }],
  settings: {
    "svelte3/typescript": () => require("typescript"),
    "svelte3/ignore-styles": () => true,
  },
  parserOptions: {
    project: "tsconfig.json",
    tsconfigRootDir: __dirname,
    sourceType: "module",
    ecmaVersion: 2019,
  },
  env: {
    browser: true,
    es2017: true,
    node: true,
  },
  rules: {
    "node/no-unpublished-import": "off",
  },
}
