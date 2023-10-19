import { PATHS } from "./constants.ts";
import { Content } from "./content.ts";
import { Site } from "./site.ts";
import * as fs from "std/fs/mod.ts";

export type ChangeFreq = "always" | "hourly" | "daily" | "weekly" | "monthly" | "yearly" | "never";

export interface UrlEntry {
  loc: URL;
  lastmod?: string;
  changefreq?: ChangeFreq;
  priority?: number;
}

export const urlFromContent = ({ url, frontmatter, contentType }: Content, base: URL): UrlEntry => {
  const loc = new URL(url, base);

  return {
    loc,
    lastmod: frontmatter.lastModified.toISOString(),
    changefreq: contentType === "page" ? "yearly" : "monthly",
    priority: contentType === "page" ? 0.8 : 0.5,
  };
};

export const renderUrlEntry = (entry: UrlEntry): string => {
  return `
<url>
  <loc>${entry.loc.toString()}</loc>
  ${entry.lastmod ? `<lastmod>${entry.lastmod}</lastmod>` : ""}
  ${entry.changefreq ? `<changefreq>${entry.changefreq}</changefreq>` : ""}
  ${entry.priority ? `<priority>${entry.priority}</priority>` : ""}
 </url>
	`
    .replace("\n  \n", "\n")
    .trim();
};

export const createSitemap = async (site: Site): Promise<void> => {
  const urls = [...site.content.values()]
    .filter((p) => !p.frontmatter.special)
    .map((page) => urlFromContent(page, site.url));

  const sitemap = `
<?xml version="1.0" encoding="UTF-8"?>
<?xml-stylesheet href="/sitemap-style.xsl" type="text/xsl"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9" xmlns:image="http://www.google.com/schemas/sitemap-image/1.1" xmlns:video="http://www.google.com/schemas/sitemap-video/1.1">
	${urls.map(renderUrlEntry).join("\n")}
</urlset>
`.trimStart();

  await fs.ensureDir(PATHS.out);
  await Deno.writeTextFile(`${PATHS.out}/sitemap.xml`, sitemap);
};
