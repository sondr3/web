module.exports = {
  parserOptions: {
    project: "./tsconfig.json",
  },
  env: {
    jest: true,
  },
  extends: [
    "@sondr3/eslint-config/typescript",
    "@sondr3/eslint-config/node",
    "@sondr3/eslint-config/functional",
    "plugin:jest/recommended",
    "plugin:jest/style",
  ],
}
