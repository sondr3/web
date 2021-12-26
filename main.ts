import { Args, parse } from "https://deno.land/std@0.119.0/flags/mod.ts";

const HELP = `web 0.0.0
A simple website

COMMANDS:
  serve, s      Serve the built website
  build, b      Build the website
  dev, d        Run in development mode
  help, h       Show this message`;

export class CLI {
  private argv: Args;

  constructor() {
    this.argv = parse(Deno.args, {
      boolean: ["verbose"],
      default: { verbose: false },
      alias: { v: "verbose" },
    });
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
    }
  }
}

const cli = new CLI();
cli.execute();
