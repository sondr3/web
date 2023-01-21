/** @type {import('eslint').Linter.Config} */
module.exports = {
  extends: [
    "@sondr3/eslint-config/typescript",
    "plugin:astro/recommended",
    "plugin:astro/jsx-a11y-strict",
  ],
  parser: "@typescript-eslint/parser",
  parserOptions: {
    project: "tsconfig.json",
    sourceType: "module",
  },
  overrides: [
    {
      files: ["*.astro"],
      parser: "astro-eslint-parser",
      parserOptions: {
        parser: "@typescript-eslint/parser",
        project: "tsconfig.json",
        sourceType: "module",
        extraFileExtensions: [".astro"],
      },
    },
  ],
};
