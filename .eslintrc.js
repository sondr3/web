module.exports = {
  parserOptions: {
    project: "./tsconfig.json",
  },
  env: {
    jest: true,
  },
  extends: ["@sondr3/eslint-config/typescript", "@sondr3/eslint-config/tsx"],
  rules: {
    "unicorn/no-array-for-each": "off",
    "react/react-in-jsx-scope": "off",
  },
  overrides: [
    {
      files: ["pages/**/*.tsx"],
      rules: {
        "import/no-default-export": "off",
      },
    },
  ],
}
