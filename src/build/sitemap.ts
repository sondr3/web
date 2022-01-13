import path from "node:path";

import { html } from "../templates/templating.js";
import { writeFile } from "../utils/fs.js";
import { Content, Frontmatter } from "./content.js";
import { Site } from "./site.js";

const modifiedAt = (fm: Frontmatter): string => {
  if (fm.modified !== null) return fm.modified.toISOString().split("T")[0];
  return fm.created?.toISOString().split("T")[0] ?? new Date().toISOString().split("T")[0];
};

const renderPage = (site: Site, content: Content): string => {
  const lastmod = modifiedAt(content.frontmatter);

  return html`
    <url>
      <loc>${site.url()}${content.url()}</loc>
      <lastmod>${lastmod}</lastmod>
      <changefreq>monthly</changefreq>
      <priority>0.7</priority>
    </url>
  `;
};

const buildSitemap = (site: Site): string => {
  return html`<?xml version="1.0" encoding="UTF-8"?>
    <urlset
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd"
      xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"
    >
      ${[...site.pages].map((element) => renderPage(site, element))}
    </urlset>`;
};

export const sitemap = async (site: Site): Promise<Error | void> => {
  const sitemap = buildSitemap(site);
  return await writeFile(path.resolve(site.config.out, "sitemap.xml"), sitemap);
};
