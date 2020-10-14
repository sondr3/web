import { dirWalk } from "./utils/fs";
import { getConfig } from "./config";
import path from "path";
import { logging } from "./utils/logging";
import { TemplateEngine } from "./template";
import { copyAssets, renderStyles } from "./assets";
import { siteState } from "./state";
import { Asciidoc } from "./Asciidoc";

const state = siteState;
const logger = logging.getLogger("build");
const config = getConfig();

const engine = new TemplateEngine();
const asciidoc = new Asciidoc();

export const buildSite = async (): Promise<void> => {
  await copyAssets();
  await renderStyles(path.join(getConfig().assets.style, "style.scss"), false);
  await renderPages();
  logger.log(state.styles[Symbol.toStringTag]);
};

export const renderPages = async (): Promise<void> => {
  const pages = await dirWalk(path.resolve(process.cwd(), config.content.pages), "liquid", false);

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

export const renderAsciidoc = async (filepath: string): Promise<string | Error> => {
  return await asciidoc.render(filepath);
};
