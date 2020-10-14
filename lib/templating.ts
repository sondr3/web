import { getConfig } from "./config";
import { logging } from "./utils/logging";
import * as templates from "./templates/layouts";

const config = getConfig();
const logger = logging.getLogger("template");

export type Layout = "default";

export interface Content {
  title: string;
  content: string;
}

export const renderTemplate = (layout: Layout, content: Content): string => {
  logger.debug(`Rendering ${String(layout)}`);

  switch (layout) {
    case "default":
      return templates.layout(createTitle(content.title), content.content);
  }
};

export const createTitle = (title: string): string => {
  if (title === config.meta.title) return title;

  return `${title} | ${config.meta.title}`;
};
