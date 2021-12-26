import * as log from "https://deno.land/std@0.119.0/log/mod.ts";

import { CLI } from "./src/mod.ts";

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

cli.execute();
