module.exports = {
  env: {
    browser: true,
    es2021: true,
  },
  extends: ["@sondr3/eslint-config/typescript", "@sondr3/eslint-config/tsx"],
  parser: "@typescript-eslint/parser",
  parserOptions: {
    project: "tsconfig.json",
    ecmaVersion: 12,
    sourceType: "module",
  },
  rules: {
    "unicorn/no-array-for-each": "off",
    "unicorn/prefer-node-protocol": "off",
    "react/react-in-jsx-scope": "off",
    "jsx-a11y/anchor-is-valid": [
      "error",
      {
        components: ["Link"],
        specialLink: ["hrefLeft", "hrefRight"],
        aspects: ["invalidHref", "preferButton"],
      },
    ],
  },
  overrides: [
    {
      files: ["pages/**/*.{ts,tsx}"],
      rules: {
        "import/no-default-export": "off",
      },
    },
  ],
}
