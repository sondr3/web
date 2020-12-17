import { getConfig } from "./config"
import { logging } from "./utils/logging"
import * as templates from "./templates/layouts"

const config = getConfig()
const logger = logging.getLogger("template")

export type Layout = "default" | "page"

export interface Content {
  title: string
  content: string
}

/**
 * Renders a HTML layout with its content.
 *
 * @param layout - Layout to render as, e.g. post, page etc
 * @param content - Content to render
 * @returns The rendered content
 */
export const renderTemplate = (layout: Layout, content: Content): string => {
  logger.debug(`Rendering ${String(layout)}`)

  switch (layout) {
    case "page":
      return templates.page(content.title, content.content)
    case "default":
      return templates.layout(createTitle(content.title), content.content)
  }
}

/**
 * Utility function to create the `<title>Blah</title` HTML tag, ensuring that the
 * landing page doesn't get a redundant title added to it.
 *
 * @param title - Content title
 * @returns The corrected title
 */
export const createTitle = (title: string): string => {
  if (title === config.meta.title) return title

  return `${title} | ${config.meta.title}`
}
