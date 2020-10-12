import sass, { Result as SassResult } from "sass";
import { logging } from "../utils/logging";
import path from "path";
import { createDirectory, writeFile } from "../utils/fs";
import { getConfig } from "../config";
import { allOk } from "../utils/utils";

const logger = logging.getLogger("sass");

export const renderStyles = (file: string, prod: boolean): Promise<void | Error> => {
  const style = sass.renderSync({
    file: file,
    sourceMap: !prod,
    outFile: styleName(file),
  });

  logger.debug(`Rendered ${file}: took ${style.stats.duration}`);

  return writeStyles(file, style);
};

const writeStyles = async (file: string, res: SassResult): Promise<void | Error> => {
  const dir = await createDirectory(path.parse(styleName(file)).dir);
  const css = await writeFile(styleName(file, "css"), res.css);
  const map = await writeFile(styleName(file, "map"), res.map ?? "");

  if (!allOk(...[dir, css, map])) return new Error("Could not create styles");

  return;
};

export const styleName = (file: string, ext?: string): string => {
  const config = getConfig();
  const { name } = path.parse(file);
  const ending = ext ? `.${ext}` : "";
  return `${config.out}/public/assets/style/${name}${ending}`;
};
