/** @type {import('eslint').Linter.Config} */
module.exports = {
  extends: [
    "@sondr3/eslint-config/base",
    "plugin:astro/recommended",
    "plugin:astro/jsx-a11y-strict",
  ],
  env: {
    es2022: true,
    browser: true,
    node: true,
  },
  parser: "@typescript-eslint/parser",
  parserOptions: {
    ecmaVersion: "latest",
    sourceType: "module",
  },
  overrides: [
    {
      files: ["*.astro"],
      parser: "astro-eslint-parser",
      parserOptions: {
        parser: "@typescript-eslint/parser",
        extraFileExtensions: [".astro"],
      },
    },
  ],
  rules: {
    "import/no-unresolved": "off",
  },
};
