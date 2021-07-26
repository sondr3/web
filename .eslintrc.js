module.exports = {
  parserOptions: {
    project: "./tsconfig.json",
  },
  env: {
    jest: true,
  },
  globals: {
    __dirname: true,
  },
  extends: [
    "@sondr3/eslint-config/typescript",
    "@sondr3/eslint-config/node",
    "plugin:jest/recommended",
    "plugin:jest/style",
  ],
  rules: {
    "unicorn/no-array-for-each": "off",
    "unicorn/prefer-node-protocol": "off",
    "unicorn/prefer-module": "off",
  },
}
