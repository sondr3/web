module.exports = {
  parser: "@typescript-eslint/parser",
  parserOptions: {
    ecmaVersion: 2018,
    sourceType: "module",
    ecmaFeatures: {
      jsx: true,
      impliedStrict: true
    },
    project: "./tsconfig.json"
  },
  env: {
    browser: true,
    node: true,
    jest: true,
    es6: true
  },
  plugins: ["@typescript-eslint", "prettier", "react-hooks", "jest"],
  extends: [
    "airbnb-base",
    "eslint:recommended",
    "plugin:react/recommended",
    "plugin:@typescript-eslint/recommended",
    "prettier",
    "prettier/@typescript-eslint",
    "prettier/react",
    "plugin:jest/recommended"
  ],
  rules: {
    "react/prop-types": "off",
    "@typescript-eslint/explicit-function-return-type": "off",
    "prettier/prettier": "error",
    "react-hooks/rules-of-hooks": "error"
  },
  settings: {
    react: {
      version: "detect"
    }
  }
};
