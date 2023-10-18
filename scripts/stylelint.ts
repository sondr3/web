import stylelint from "npm:stylelint@^15.11";
import "npm:prettier@^3";
import "npm:stylelint-prettier@^4";
import "npm:stylelint-config-standard-scss@^11";

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

console.error(res.output);

if (res.errored) {
  Deno.exit(1);
}
