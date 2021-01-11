import path from "path"
import { EitherAsync } from "purify-ts/EitherAsync"

import { getConfig } from "../config"
import * as pages from "../templates/pages"
import { FSError, walkDirectory } from "../utils"
import { addPage, convertAsciidoc, renderAsciidoc, writeContent, writeHTML } from "."
import { convertDate } from "./helpers"

const config = getConfig()

/**
 * Render all pages in the `pages` directory in {@link config}.
 *
 * @param production - Whether to optimize output
 */
export const renderPages = (production: boolean): EitherAsync<FSError, void> =>
  EitherAsync(async () => {
    const pages = await walkDirectory(path.resolve(process.cwd(), config.content.pages), "adoc", false)

    pages.map((page) => {
      void convertAsciidoc(page)
        .map(async (document) => {
          const rendered = renderAsciidoc(document)

          addPage({
            title: document.getTitle(),
            path: <string>document.getAttribute("path", `/${path.parse(page).name}/`),
            description: document.getCaptionedTitle(),
            createdAt: convertDate(document.getAttribute("created_at")),
            modifiedAt: convertDate(document.getAttribute("modified_at")),
          })

          const file = path.parse(page)
          await writeContent(path.resolve(config.out, file.name), writeHTML(rendered, production))
        })
        .mapLeft((error) => error)
        .run()
    })
  })

/**
 * Renders "special" pages, e.g. landing page, 404 and such.
 *
 * @param production - Whether to optimize output
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
