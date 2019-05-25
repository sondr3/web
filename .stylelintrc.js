module.exports = {
  plugins: ["stylelint-order"],
  extends: [
    "stylelint-config-recommended",
    "stylelint-a11y/recommended",
    "stylelint-prettier/recommended"
  ],
  rules: {
    "order/order": ["custom-properties", "declarations"],
    "order/properties-alphabetical-order": true
  }
};
