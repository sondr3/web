import { promises as fs } from "node:fs";

import { build } from "./build/build.js";
import { CLI } from "./cli.js";
import { context } from "./context.js";
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
  const ctx = await context(cli.production);

  switch (cli.command) {
    case "build": {
      console.info(`Building with ${cli.production ? "optimizations" : "no optimizations"}`);
      await build(ctx);
      return;
    }
    case "dev": {
      console.info(`Building with ${cli.production ? "optimizations" : "no optimizations"}`);
      await build(ctx);
      console.info(`Starting development server...`);
      const server = new Server(ctx);
      server.start();

      process.on("SIGINT", () => shutdown(server));
      return;
    }
    case "clean": {
      console.info(`Cleaning out ${ctx.config.out}`);
      await fs.rm(ctx.config.out, { recursive: true, force: true });
      return;
    }
  }
};
