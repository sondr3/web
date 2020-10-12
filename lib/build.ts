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
      const output = await engine.renderFile(file.name);
      await createDirectory(config.out);
      await writeFile(path.join(config.out, "index.html"), output);
    } else {
      const output = await engine.renderFile(file.name);
      const dir = path.join(config.out, file.name);
      await createDirectory(dir);
      await writeFile(path.join(dir, "index.html"), output);
    }
  }

  return;
};
