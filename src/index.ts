import { Asciidoc } from "./asciidoc.js";
import { CLI } from "./cli.js";
import { renderPages } from "./content.js";

/**
 * Entrypoint for static site generator, parses command line input and run
 * the given command.
 */
export const run = async (): Promise<void> => {
  const cli = new CLI(process.argv);

  switch (cli.command) {
    case "build": {
      const asciidoc = new Asciidoc();
      await renderPages(asciidoc).run();
      return;
    }
    case "serve":
      return console.log("serve");
    case "clean": {
      return console.log("clean");
    }
  }
};
