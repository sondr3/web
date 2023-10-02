import stylelint from "npm:stylelint@^15.10";
import "npm:stylelint-prettier@^4";
import "npm:stylelint-config-standard-scss@^1";

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
