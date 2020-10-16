import { buildSite } from "./build"
import { logging } from "./utils/logging"
import { Server } from "./server"
import { CLI } from "./utils/CLI"

const run = async (): Promise<void> => {
  const cli = new CLI(process.argv)

  logging.configure({ minLevel: cli.noisiness }).registerConsoleLogger()

  const logger = logging.getLogger("index")
  const server = new Server()

  switch (cli.command) {
    case "build":
      await buildSite(cli.production)
      break
    case "develop": {
      await buildSite(cli.production)
      server.run()
      break
    }
    case "serve":
      server.serve()
      break
    case "clean":
      break
  }

  process.on("SIGINT", () => {
    logger.log(`Shutting down...`)
    server.broadcastShutdown()
    // eslint-disable-next-line no-process-exit
    process.exit()
  })
}

void run()
