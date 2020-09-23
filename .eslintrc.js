module.exports = {
  parserOptions: {
    ecmaVersion: 2020,
    project: "./tsconfig.json",
    sourceType: "module",
  },
  extends: ["@sondr3/typescript", "plugin:node/recommended-module"],
};
