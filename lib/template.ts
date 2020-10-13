import { Liquid } from "liquidjs";
import { getConfig } from "./config";
import path from "path";
import { createDirectory, writeFile } from "./utils/fs";
import { logging } from "./utils/logging";
import assert from "assert";

const config = getConfig();
const logger = logging.getLogger("template");

export class TemplateEngine {
  private engine: Liquid;

  constructor() {
    this.engine = new Liquid({
      root: [
        config.templates.layouts,
        config.templates.partials,
        config.content.pages,
        config.templates.pages,
        config.templates.asciidoc,
      ],
      extname: ".liquid",
    });

    this.engine.registerFilter("titlify", titlify);
  }

  async render(page: string, directory: string): Promise<void> {
    logger.debug(`Rendering ${page} to ${directory}`);
    const file = path.parse(page);
    const output = await this.engine.renderFile(file.name, {
      title: "Test",
    });
    await createDirectory(directory);
    await writeFile(path.join(directory, "index.html"), output);
  }

  renderAsciidocTemplate(template: string, data: Record<string, string>): string {
    return this.engine.renderFileSync(template, data);
  }
}

const titlify = (title: string): string => {
  assert(title !== undefined, "Title must be a string");
  if (title === config.meta.title) return title;

  return `${title} | ${config.meta.title}`;
};
