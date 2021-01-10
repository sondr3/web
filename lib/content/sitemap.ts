import path from "path"

import { getConfig } from "../config"
import { siteState } from "../state"
import { html } from "../templates"
import { formatHTML, writeFile } from "../utils"
import { Metadata } from "."

const config = getConfig()
const state = siteState

/**
 * Get the last modified at date or the creation date if that doesn't exist,
 * returning `null` if neither is found.
 *
 * @param page - Page to get last modified at
 * @returns The formatted date or null
 */
const modifiedAt = (page: Metadata): string => {
  if (page.modifiedAt) return page.modifiedAt.toISOString().split("T")[0]
  return page.createdAt?.toISOString().split("T")[0] ?? new Date().toISOString().split("T")[0]
}

/**
 * Render the sitemap entry for a page.
 *
 * @param page - Page to render
 * @returns A XML entry
 */
const renderPage = (page: Metadata): string => {
  const lastmod = modifiedAt(page)
  return html`
    <url>
      <loc>${config.meta.url}${page.path}</loc>
      <lastmod>${lastmod}</lastmod>
      <changefreq>monthly</changefreq>
      <priority>0.7</priority>
    </url>
  `
}

/**
 * Build the sitemap for the website.
 *
 * @returns The sitemap
 */
const buildSitemap = (): string => {
  return html`<?xml version="1.0" encoding="UTF-8"?>
    <urlset
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd"
      xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"
    >
      ${[...state.pages.values()].map(renderPage)}
    </urlset>`
}

/**
 * Build and write the sitemap to the out directory.
 */
export const sitemap = async (): Promise<void> => {
  const sitemap = buildSitemap()
  await writeFile(path.resolve(config.out, "sitemap.xml"), formatHTML(sitemap))
}
