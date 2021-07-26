import { CLI } from "./src/mod.ts";
import * as log from "std/log/mod.ts";

const cli = new CLI();

await log.setup({
  handlers: {
    console: new log.handlers.ConsoleHandler(cli.noisiness()),
  },
  loggers: {
    default: {
      level: cli.noisiness(),
      handlers: ["console"],
    },
  },
});

void cli.execute();
