import { buildSite } from "./build";
import { logging } from "./utils/logging";

const run = async (): Promise<void> => {
  const mode = process.env.NODE_ENV ?? "development";
  logging.configure().registerConsoleLogger();

  if (mode === "prod") {
    await buildSite()
      .then(() => void {})
      .catch((err) => console.error(err));
  } else {
    await buildSite()
      .then(() => void {})
      .catch((err) => console.error(err));
  }
};

void run();
