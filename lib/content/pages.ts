import { dirWalk } from "../utils"
import path from "path"
import * as pages from "../templates/pages"
import { getConfig } from "../config"
import { addPage, convertAsciidoc, renderAsciidoc, writeContent, writeHTML } from "./"
import { convertDate } from "./helpers"

const config = getConfig()

/**
 * Render all pages in the `pages` directory in {@link config}.
 *
 * @param prod - Whether to optimize output
 */
export const renderPages = async (prod: boolean): Promise<void | Error> => {
  const pages = await dirWalk(path.resolve(process.cwd(), config.content.pages), "adoc", false)

  for (const page of pages) {
    const doc = await convertAsciidoc(page)
    if (doc instanceof Error) return doc
    const rendered = renderAsciidoc(doc)
    if (rendered instanceof Error) return rendered

    addPage({
      title: doc.getTitle(),
      path: <string>doc.getAttribute("path", `/${path.parse(page).name}/`),
      description: doc.getCaptionedTitle(),
      createdAt: convertDate(doc.getAttribute("created_at", undefined)),
      modifiedAt: convertDate(doc.getAttribute("modified_at", undefined)),
    })

    const file = path.parse(page)
    await writeContent(path.resolve(config.out, file.name), writeHTML(rendered, prod))
  }
}

/**
 * Renders "special" pages, e.g. landing page, 404 and such.
 *
 * @param prod - Whether to optimize output
 */
export const renderSpecialPages = async (prod: boolean): Promise<void> => {
  await writeContent(config.out, writeHTML(pages.landing(), prod))
  addPage({
    title: "Eons",
    path: "/",
    description: "Webpage for Sondre Nilsen",
    createdAt: new Date("2020-12-18"),
  })

  await writeContent(path.resolve(config.out, "404/"), writeHTML(pages.notFound(), prod))
  addPage({
    title: "404",
    path: "/404/",
    description: "Page not found",
    createdAt: new Date("2020-12-18"),
  })
}
