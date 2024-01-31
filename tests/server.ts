import { logConfig } from "../src/logger.js";
import { httpServer } from "../src/server.js";

logConfig.level = "info";
httpServer();
