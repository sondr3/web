import { promises as fs } from "node:fs";

import { build } from "./build/build.js";
import { CLI } from "./cli.js";
import { context } from "./context.js";
import { Server } from "./server.js";
import * as logger from "./utils/logger.js";

/**
 * Stop the running server "nicely"
 */
const shutdown = (server: Server) => {
  logger.warn(`Shutting down...`);
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
      logger.info(`Building with ${cli.production ? "optimizations" : "no optimizations"}`);
      await build(ctx);
      return;
    }
    case "dev": {
      logger.info(`Building with ${cli.production ? "optimizations" : "no optimizations"}`);
      await build(ctx);
      logger.info(`Starting development server...`);
      const server = new Server(ctx);
      server.start();

      process.on("SIGINT", () => shutdown(server));
      return;
    }
    case "clean": {
      logger.info(`Cleaning out ${ctx.config.out}`);
      await fs.rm(ctx.config.out, { recursive: true, force: true });
      return;
    }
  }
};
