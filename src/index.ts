import { Asciidoc } from "./build/asciidoc.js";
import { build } from "./build/build.js";
import { Site } from "./build/site.js";
import { CLI } from "./cli.js";
import { Server } from "./server.js";

/**
 * Stop the running server "nicely"
 */
const shutdown = (server: Server) => {
  console.log(`Shutting down...`);
  server.close();
  process.exit();
};

/**
 * Entrypoint for static site generator, parses command line input and run
 * the given command.
 */
export const run = async (): Promise<void> => {
  const cli = new CLI(process.argv);
  const site = new Site(cli.production);
  const asciidoc = new Asciidoc();

  switch (cli.command) {
    case "build": {
      console.info(`Building with ${cli.production ? "optimizations" : "no optimizations"}`);
      await build(site, asciidoc);
      return;
    }
    case "dev": {
      console.info(`Building with ${cli.production ? "optimizations" : "no optimizations"}`);
      await build(site, asciidoc);
      console.info(`Starting development server...`);
      const server = new Server(site, asciidoc);
      server.start();

      process.on("SIGINT", () => shutdown(server));
      return;
    }
    case "clean": {
      return console.log("clean");
    }
  }
};
