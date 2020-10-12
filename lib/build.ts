import { createDirectory, dirWalk, writeFile } from "./utils/fs";
import { Liquid } from "liquidjs";
import { getConfig } from "./config";
import path from "path";
import { logging } from "./utils/logging";

const logger = logging.getLogger("build");
const config = getConfig();

const engine = new Liquid({
  root: [config.content.pages, config.content.layouts, config.content.partials],
  extname: ".liquid",
});

export const renderPages = async (): Promise<void> => {
  const pages = await dirWalk(path.join(process.cwd(), "/content/pages"), "liquid", false);

  for (const page of pages) {
    logger.debug(`Building page: ${path.resolve(page)}`);
    const file = path.parse(page);

    if (file.name === "index") {
      await renderPage(page, config.out);
    } else {
      const dir = path.join(config.out, file.name);
      await renderPage(page, dir);
    }
  }

  return;
};

export const renderPage = async (page: string, directory: string): Promise<void> => {
  const file = path.parse(page);
  const output = await engine.renderFile(file.name);
  await createDirectory(directory);
  await writeFile(path.join(directory, "index.html"), output);
};
