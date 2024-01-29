import stylelint from "npm:stylelint@^16.2";
import "npm:prettier@^3";
import "npm:stylelint-prettier@^5";
import "npm:stylelint-config-standard-scss@^13";

const res = await stylelint.lint({
  config: {
    extends: [
      "stylelint-prettier/recommended",
      "stylelint-config-standard-scss",
    ],
  },
  files: "./site/styles/**/*.scss",
  fix: !Deno.env.has("CI"),
  cache: true,
  formatter: "string",
});

console.error(res.report);

if (res.errored) {
  Deno.exit(1);
}
