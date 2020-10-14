import { createDirectory, writeFile } from "./utils/fs";
import { getConfig } from "./config";
import path from "path";
import { logging } from "./utils/logging";
import { Layout, renderTemplate } from "./templating";
import { copyAssets, renderStyles } from "./assets";
import { siteState } from "./state";
import { Asciidoc } from "./Asciidoc";
import * as pages from "./templates/pages";

const state = siteState;
const logger = logging.getLogger("build");
const config = getConfig();

const asciidoc = new Asciidoc();

export const buildSite = async (): Promise<void> => {
  await copyAssets();
  await renderStyles(path.join(getConfig().assets.style, "style.scss"), false);
  await renderSpecialPages();
  logger.log(state.styles[Symbol.toStringTag]);
};

export const renderSpecialPages = async (): Promise<void> => {
  await writeContent(config.out, pages.landing());
  await writeContent(path.resolve(config.out, "404/"), pages.notFound());
};

export const renderAsciidoc = async (filepath: string): Promise<string | Error> => {
  const content = await asciidoc.load(filepath);
  if (content instanceof Error) return content;

  const layout = content.getAttribute("layout", "Default") as Layout;
  return renderTemplate(layout, { title: content.getTitle(), content: content.getContent() });
};

export const writeContent = async (directory: string, content: string): Promise<void> => {
  await createDirectory(directory);
  await writeFile(path.join(directory, "index.html"), content);
};
