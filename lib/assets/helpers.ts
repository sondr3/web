import { getConfig } from "../config";
import { copyFiles } from "../utils/fs";
import path from "path";
import { promises as fs } from "fs";
import { logging } from "../utils/logging";

const logger = logging.getLogger("assets");
const config = getConfig();

export const copyAssets = async (): Promise<void> => {
  logger.debug("Copying assets");
  await fs.rmdir(path.join(config.out, "static"), { recursive: true });
  await copyFiles(config.assets.static, path.join(config.out, "static"));
  logger.debug("Copying assets finished");
};
