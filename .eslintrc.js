module.exports = {
  parserOptions: {
    project: "./tsconfig.json"
  },
  extends: ["@sondr3/typescript", "@sondr3/react", "@sondr3/react/typescript"],
  rules: {
    "@typescript-eslint/strict-boolean-expressions": ["off"],
    "@typescript-eslint/no-unnecessary-condition": ["error", { ignoreRhs: true }]
  }
};
