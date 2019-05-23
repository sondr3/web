module.exports = {
  parser: "@typescript-eslint/parser",
  parserOptions: {
    ecmaVersion: 2018,
    sourceType: "module",
    project: "./tsconfig.json"
  },
  env: {
    browser: true,
    node: true,
    es6: true
  },
  plugins: ["@typescript-eslint", "react-hooks"],
  extends: [
    "airbnb",
    "eslint:recommended",
    "plugin:react/recommended",
    "plugin:@typescript-eslint/recommended",
    "plugin:import/errors",
    "plugin:import/warnings",
    "plugin:import/typescript",
    "plugin:prettier/recommended",
    "prettier/@typescript-eslint",
    "prettier/react"
  ],
  rules: {
    "react/jsx-filename-extension": "off",
    "react/prop-types": "off",
    "@typescript-eslint/explicit-function-return-type": "off",
    "react-hooks/rules-of-hooks": "error",
    "react-hooks/exhaustive-deps": "warn",
    "import/no-unresolved": "error"
  },
  settings: {
    "import/resolver": {
      typescript: {}
    },
    react: {
      version: "detect"
    }
  }
};
