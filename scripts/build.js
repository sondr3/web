/* eslint-disable no-console */

const execSync = require("child_process").execSync;

execSync("rm -rf public", { stdio: "inherit" });

const { URL } = process.env;

switch (URL) {
  case "https://www.eons.io":
    execSync("yarn workspace portfolio deploy", { stdio: "inherit" });
    break;
  default:
    console.error("No SITE_ID specified, exiting...");
    break;
}
