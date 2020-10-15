import { buildSite } from "./build"
import { logging } from "./utils/logging"
import { Server } from "./server"

const run = async (): Promise<void> => {
  const mode = process.env.NODE_ENV ?? "development"
  logging.configure().registerConsoleLogger()

  const logger = logging.getLogger("index")
  const prod = mode === "production"

  if (prod) {
    await buildSite(prod)
      .then(() => void {})
      .catch((err) => console.error(err))
  } else {
    await buildSite(prod)
      .then(() => void {})
      .catch((err) => console.error(err))
    const server = new Server()
    server.run()

    process.on("SIGINT", () => {
      logger.log(`Shutting down...`)
      server.broadcastShutdown()
      // eslint-disable-next-line no-process-exit
      process.exit()
    })
  }
}

void run()
