/* eslint-disable no-console */

const execSync = require("child_process").execSync;

execSync("rm -rf public", { stdio: "inherit" });

const { URL } = process.env;

console.log(`URL: ${URL}`);

switch (URL) {
  case "https://www.eons.io":
    execSync("yarn workspace portfolio deploy", { stdio: "inherit" });
    break;
  case "https://amazing-tereshkova-64abb0.netlify.com":
    execSync("yarn workspace portfolio deploy", { stdio: "inherit" });
    break;
  default:
    console.error("No URL specified, exiting...");
    break;
}
