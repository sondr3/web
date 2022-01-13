import { Asciidoc } from "./build/asciidoc.js";
import { build } from "./build/build.js";
import { Site } from "./build/site.js";
import { CLI } from "./cli.js";
import { Server } from "./server.js";

/**
 * Entrypoint for static site generator, parses command line input and run
 * the given command.
 */
export const run = async (): Promise<void> => {
  const cli = new CLI(process.argv);
  const site = new Site(cli.production);

  switch (cli.command) {
    case "build": {
      console.info(`Building with ${cli.production ? "optimizations" : "no optimizations"}`);
      await build(site, new Asciidoc());
      return;
    }
    case "dev": {
      console.info(`Starting development server...`);
      const server = new Server(site);
      server.start();
      return;
    }
    case "clean": {
      return console.log("clean");
    }
  }
};
