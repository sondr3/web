import log from "loglevel";
import { httpServer } from "../src/server.js";

log.setDefaultLevel("info");
httpServer();
