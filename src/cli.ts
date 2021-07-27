import { Args, parse } from "std/flags/mod.ts";
import { getLevelName, LevelName, LogLevels } from "std/log/levels.ts";

const HELP_MESSAGE = `site

A stupid simple static site generator

USAGE:
  site

COMMANDS:
  build, b          Build dotfiles
  help, h           Show help

OPTIONS:
  -n, --noise       Logger noisiness
  -p, --production  Run with optimizations
`;

export class CLI {
  private argv: Args;

  constructor() {
    this.argv = parse(Deno.args, {
      string: ["noise"],
      boolean: ["production"],
      default: { production: false },
      alias: { p: "production", n: "noise" },
    });
  }

  production(): boolean {
    return this.argv.production;
  }

  noisiness(): LevelName {
    const noise: string = this.argv.noise?.toLowerCase() ?? "info";
    switch (noise) {
      case "info":
        return getLevelName(LogLevels.INFO);
      case "critical":
        return getLevelName(LogLevels.CRITICAL);
      case "error":
        return getLevelName(LogLevels.ERROR);
      case "warning":
        return getLevelName(LogLevels.WARNING);
      case "debug":
      default:
        return getLevelName(LogLevels.DEBUG);
    }
  }

  printHelp() {
    console.log(HELP_MESSAGE);
    return Deno.exit();
  }

  execute() {
    switch (this.argv._[0]) {
      case "h":
      case "help": {
        return this.printHelp();
      }
      case "b":
      case "build": {
        console.log("I AM BUILDING!");
        break;
      }
      default: {
        return this.printHelp();
      }
    }
  }
}
