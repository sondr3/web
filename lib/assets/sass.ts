import sass, { Result } from "sass";
import { logging } from "../utils/logging";
import { promises as fs } from "fs";
import { EitherAsync, fromPromise, liftPromise } from "purify-ts/EitherAsync";
import path from "path";
import { createDirectory } from "../utils/fs";

const logger = logging.getLogger("sass");

export const renderStyles = (file: string, prod: boolean): EitherAsync<Error, void> => {
  const style = sass.renderSync({
    file: file,
    sourceMap: !prod,
    outFile: styleName(file),
  });

  logger.debug(`Rendered ${file}: took ${style.stats.duration}`);

  return writeStyles(file, style, prod).mapLeft(({ message }) => Error(`Could not create styles: ${message}`));
};

const writeStyles = (file: string, res: Result, prod: boolean): EitherAsync<Error, void> =>
  fromPromise(() => createDirectory(path.parse(styleName(file)).dir).run())
    .chain(() => liftPromise(() => fs.writeFile(styleName(file, "css"), res.css)))
    .chain(() => liftPromise(async () => (!prod ? fs.writeFile(styleName(file, "map"), res.map ?? "") : void {})))
    .mapLeft(({ message }) => Error(`Could not create styles: ${message}`));

const styleName = (file: string, ext?: string): string => {
  const { name } = path.parse(file);
  const ending = ext ? `.${ext}` : "";
  return `./assets/style/${name}${ending}`;
};
