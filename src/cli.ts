import { Args, parse } from "flags/mod.ts";
import { getLevelName, LevelName, LogLevels } from "log/levels.ts";

const HELP = `web 0.0.0
A simple website

COMMANDS:
  serve, s         Serve the built website
  build, b         Build the website
  dev, d           Run in development mode
  help, h          Show this message

OPTIONS
  -n, --noise      Noisiness
  -p, --production Production mode `;

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
    const noise = this.argv.noise?.toLowerCase() ?? "info";
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

  execute() {
    switch (this.argv._[0]) {
      case "h":
      case "help":
        console.log(HELP);
        return Deno.exit();
      case "b":
      case "build":
        console.log("BUILDING");
        return Deno.exit();
      case "d":
      case "dev":
        console.log("DEVELOPING");
        return Deno.exit();
      case "s":
      case "serve":
        console.log("SERVING");
        return Deno.exit();
      default:
        console.log(HELP);
        return Deno.exit();
    }
  }
}
