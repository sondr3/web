import { Config } from "../config"
import { logging } from "../logging"
import * as templates from "../templates"

const logger = logging.getLogger("template")

export type Layout = "default" | "page"

export type Content = {
  readonly title: string
  readonly content: string
}

/**
 * Renders a HTML layout with its content.
 *
 * @param config - Build configuration
 * @param layout - Layout to render as, e.g. post, page etc
 * @param content - Content to render
 * @returns The rendered content
 */
export const renderTemplate = (config: Config, layout: Layout, content: Content): string => {
  logger.debug(`Rendering ${String(layout)}`)

  switch (layout) {
    case "page":
      return templates.page(content.title, content.content)
    case "default":
      return templates.layout(createTitle(config, content.title), content.content)
  }
}

/**
 * Utility function to create the `<title>Blah</title` HTML tag, ensuring that the
 * landing page doesn't get a redundant title added to it.
 *
 * @param config - Build configuration
 * @param title - Content title
 * @returns The corrected title
 */
export const createTitle = (config: Config, title: string): string => {
  if (title === config.meta.title) return title

  return `${title} | ${config.meta.title}`
}
