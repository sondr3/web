import { promises as fs } from "node:fs";
import path from "node:path";

import { Context } from "../context.js";
import { html } from "../templates/html.js";
import { Config } from "./config.js";
import { Content, Frontmatter } from "./content.js";
import { Site } from "./site.js";

const modifiedAt = (fm: Frontmatter): string => {
  if (fm.modified !== null) return fm.modified.toISOString().split("T")[0];
  return fm.created?.toISOString().split("T")[0] ?? new Date().toISOString().split("T")[0];
};

const renderPage = (config: Config, content: Content): string => {
  const lastMod = modifiedAt(content.frontmatter);

  return html`
    <url>
      <loc>${config.url}${content.url()}</loc>
      <lastmod>${lastMod}</lastmod>
      <changefreq>monthly</changefreq>
      <priority>0.7</priority>
    </url>
  `;
};

const buildSitemap = (site: Site, config: Config): string => {
  return html`<?xml version="1.0" encoding="UTF-8"?>
    <urlset
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd"
      xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"
    >
      ${site.content().map((element) => renderPage(config, element))}
    </urlset>`;
};

export const sitemap = async ({ site, config }: Context): Promise<void> => {
  const sitemap = buildSitemap(site, config);
  return await fs.writeFile(path.resolve(config.out, "sitemap.xml"), sitemap);
};
