import path from "path"

import { getConfig } from "../config"
import * as pages from "../templates/pages"
import { dirWalk } from "../utils"
import { addPage, convertAsciidoc, renderAsciidoc, writeContent, writeHTML } from "."
import { convertDate } from "./helpers"

const config = getConfig()

/**
 * Render all pages in the `pages` directory in {@link config}.
 *
 * @param prod - Whether to optimize output
 */
export const renderPages = async (production: boolean): Promise<void | Error> => {
  const pages = await dirWalk(path.resolve(process.cwd(), config.content.pages), "adoc", false)

  for (const page of pages) {
    const document = await convertAsciidoc(page)
    if (document instanceof Error) return document
    const rendered = renderAsciidoc(document)
    if (rendered instanceof Error) return rendered

    addPage({
      title: document.getTitle(),
      path: <string>document.getAttribute("path", `/${path.parse(page).name}/`),
      description: document.getCaptionedTitle(),
      createdAt: convertDate(document.getAttribute("created_at")),
      modifiedAt: convertDate(document.getAttribute("modified_at")),
    })

    const file = path.parse(page)
    await writeContent(path.resolve(config.out, file.name), writeHTML(rendered, production))
  }
}

/**
 * Renders "special" pages, e.g. landing page, 404 and such.
 *
 * @param prod - Whether to optimize output
 */
export const renderSpecialPages = async (production: boolean): Promise<void> => {
  await writeContent(config.out, writeHTML(pages.landing(), production))
  addPage({
    title: "Eons",
    path: "/",
    description: "Webpage for Sondre Nilsen",
    createdAt: new Date("2020-12-18"),
  })

  await writeContent(path.resolve(config.out, "404/"), writeHTML(pages.notFound(), production))
  addPage({
    title: "404",
    path: "/404/",
    description: "Page not found",
    createdAt: new Date("2020-12-18"),
  })
}
