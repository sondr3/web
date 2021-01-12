import { buildSite, clean } from "./build"
import { Logger, logging } from "./logging"
import { Server } from "./server"
import { defaultConfig, setConfig } from "./site"
import { CLI } from "./utils"

/**
 * Builds the site by without serving it.
 *
 * @param cli - Command line arguments
 */
const build = async (cli: CLI): Promise<void> => {
  const config = setConfig(defaultConfig, {
    out: "./public",
    production: cli.production,
    meta: { url: "https://www.eons.io" },
  })
  await buildSite(config, cli.production).run()
}

/**
 * Build and serves the site.
 *
 * @param cli - Command line arguments
 * @param logger - Logger to output to
 */
const develop = async (cli: CLI, logger: Logger): Promise<void> => {
  const config = setConfig(defaultConfig, {
    out: "./public",
    production: cli.production,
    meta: { url: "http://localhost" },
  })
  await buildSite(config, cli.production)
  const server = new Server(config)
  server.run()

  process.on("SIGINT", () => shutdown(server, logger))
}

/**
 * Starts the server by only serving the `./public` directory.
 *
 * @param logger - Logger to output to
 */
const serve = (logger: Logger): void => {
  const server = new Server(defaultConfig)
  server.serve()

  process.on("SIGINT", () => shutdown(server, logger))
}

/**
 * Utility function to broadcast shutdown and close the server.
 *
 * @param server - Server to close
 * @param logger - Logger to output to
 */
const shutdown = (server: Server, logger: Logger): void => {
  logger.log(`Shutting down...`)
  server.broadcastShutdown()
  server.close()
  process.kill(process.pid)
}

/**
 * Entrypoint for static site generator, parses command line input and run
 * the given command.
 */
const run = async (): Promise<void> => {
  const cli = new CLI(process.argv)

  logging.configure({ minLevel: cli.noisiness }).registerConsoleLogger()

  const logger = logging.getLogger("index")

  switch (cli.command) {
    case "build":
      return await build(cli)
    case "develop":
      return await develop(cli, logger)
    case "serve":
      return serve(logger)
    case "clean": {
      const config = setConfig(defaultConfig, { production: false, out: "./public" })
      return await clean(config)
    }
  }
}

void run()
