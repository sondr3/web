import { Asciidoc } from "./build/asciidoc.js";
import { build } from "./build/build.js";
import { Site } from "./build/site.js";
import { CLI } from "./cli.js";

/**
 * Entrypoint for static site generator, parses command line input and run
 * the given command.
 */
export const run = async (): Promise<void> => {
  const cli = new CLI(process.argv);

  switch (cli.command) {
    case "build": {
      await build(new Site(cli.production), new Asciidoc())
        .mapLeft((e) => {
          throw e;
        })
        .run();
      return;
    }
    case "serve":
      return console.log("serve");
    case "clean": {
      return console.log("clean");
    }
  }
};
