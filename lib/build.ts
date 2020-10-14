import { createDirectory, dirWalk, writeFile } from "./utils/fs";
import { getConfig } from "./config";
import path from "path";
import { logging } from "./utils/logging";
import { Layout, renderTemplate } from "./templating";
import { copyAssets, renderStyles } from "./assets";
import { Asciidoc } from "./Asciidoc";
import * as pages from "./templates/pages";
import { formatHtml } from "./utils/formatting";
import { Duration } from "./utils/Duration";

const logger = logging.getLogger("build");
const config = getConfig();

const asciidoc = new Asciidoc();

export const buildSite = async (prod: boolean): Promise<void> => {
  logger.log(`Building site ${config.meta.title} (${config.meta.url})`);
  const duration = new Duration();
  await copyAssets();
  await renderStyles(path.join(getConfig().assets.style, "style.scss"), prod);
  await renderSpecialPages();
  await renderPages(prod);
  duration.end();
  logger.log(`Took ${duration.result()} to build site`);
};

export const renderPages = async (prod: boolean): Promise<void | Error> => {
  const pages = await dirWalk(path.resolve(process.cwd(), config.content.pages), "adoc", false);

  for (const page of pages) {
    const rendered = await renderAsciidoc(page, prod);
    if (rendered instanceof Error) return rendered;
    const file = path.parse(page);
    await writeContent(path.resolve(config.out, file.name), formatHtml(rendered));
  }
};

export const renderSpecialPages = async (): Promise<void> => {
  await writeContent(config.out, formatHtml(pages.landing()));
  await writeContent(path.resolve(config.out, "404/"), formatHtml(pages.notFound()));
};

export const renderAsciidoc = async (filepath: string, prod: boolean): Promise<string | Error> => {
  const content = await asciidoc.load(filepath);
  if (content instanceof Error) return content;

  const layout = content.getAttribute("layout", "default") as Layout;
  const rendered = renderTemplate(layout, { title: content.getTitle(), content: content.getContent() });
  if (!prod) return formatHtml(rendered);

  return rendered;
};

export const writeContent = async (directory: string, content: string): Promise<void> => {
  await createDirectory(directory);
  await writeFile(path.join(directory, "index.html"), content);
};
