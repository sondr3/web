import { buildSite } from "./build"
import { logging } from "./utils/logging"
import { Server } from "./server"

const run = async (): Promise<void> => {
  const mode = process.env.NODE_ENV ?? "development"
  logging.configure().registerConsoleLogger()

  const prod = mode === "production"

  if (mode === "prod") {
    await buildSite(prod)
      .then(() => void {})
      .catch((err) => console.error(err))
  } else {
    await buildSite(prod)
      .then(() => void {})
      .catch((err) => console.error(err))
    new Server().run()
  }
}

void run()
