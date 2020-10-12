import { getConfig } from "../config";
import { copyFiles } from "../utils/fs";
import path from "path";
import { promises as fs } from "fs";

const config = getConfig();

export const copyAssets = async (): Promise<void> => {
  await fs.rmdir(path.join(config.out, "static"), { recursive: true });
  await copyFiles(config.assets.static, path.join(config.out, "static"));
};
