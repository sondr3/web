import { dirWalk } from "./utils/fs";
import { getConfig } from "./config";
import path from "path";
import { logging } from "./utils/logging";
import { TemplateEngine } from "./template";
import { renderStyles } from "./assets";

const logger = logging.getLogger("build");
const config = getConfig();

const engine = new TemplateEngine();

export const buildSite = async (): Promise<void> => {
  await renderStyles(path.join(getConfig().assets.style, "style.scss"), false);
  await renderPages();
};

export const renderPages = async (): Promise<void> => {
  const pages = await dirWalk(path.join(process.cwd(), "/content/pages"), "liquid", false);

  for (const page of pages) {
    logger.debug(`Building page: ${path.resolve(page)}`);
    const file = path.parse(page);

    if (file.name === "index") {
      await engine.render(page, config.out);
    } else {
      const dir = path.join(config.out, file.name);
      await engine.render(page, dir);
    }
  }

  return;
};
