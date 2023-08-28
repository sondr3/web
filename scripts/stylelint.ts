import stylelint from "stylelint";
import "stylelint-prettier";
import "stylelint-scss";

const res = await stylelint.lint({
  config: {
    extends: [
      "stylelint-prettier/recommended",
      "stylelint-config-standard-scss",
    ],
  },
  files: "./site/styles/**/*.scss",
  fix: true,
  cache: true,
  formatter: "string",
});

console.error(res.output);

if (res.errored) {
  Deno.exit(1);
}
