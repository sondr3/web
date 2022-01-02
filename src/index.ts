import { CLI } from "./cli.js"

/**
 * Entrypoint for static site generator, parses command line input and run
 * the given command.
 */
const run = (): void => {
  const cli = new CLI(process.argv)

  switch (cli.command) {
    case "build":
      return console.log("building")
    case "serve":
      return console.log("serve")
    case "clean": {
      return console.log("clean")
    }
  }
}

void run()
