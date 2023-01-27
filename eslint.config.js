/** @type {import('eslint').Linter.Config[]} */
export default [
  {
    files: ["src/**/*", "tests/**/*"],
    ignores: ["**/*.md", "**/*.mdx"],
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
      {
        files: ["*.ts"],
        parser: "@typescript-eslint/parser",
        parserOptions: {
          project: "tsconfig.json",
        },
        extends: ["@sondr3/eslint-config/typescript"],
      },
      {
        // Define the configuration for `<script>` tag.
        // Script in `<script>` is assigned a virtual file name with the `.js` extension.
        files: ["**/*.astro/*.js", "*.astro/*.js"],
        parser: "@typescript-eslint/parser",
      },
    ],
    rules: {
      "import/no-unresolved": "off",
    },
  },
];
