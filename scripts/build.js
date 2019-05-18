/* eslint-disable no-console */

const execSync = require("child_process").execSync;

execSync("rm -rf public", { stdio: "inherit" });

const { SITE_ID } = process.env;

switch (SITE_ID) {
  case "portfolio":
    execSync("yarn workspace www build", { stdio: "inherit" });
    break;
  default:
    console.error("No SITE_ID specified, exiting...");
    break;
}
