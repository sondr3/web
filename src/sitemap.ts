import * as fs from "node:fs/promises";
import { renderSitemap } from "@sondr3/radiant/sitemap";
import { PATHS } from "./constants.js";
import { Content } from "./content.js";
import { Site } from "./site.js";
import { sitemapStyle } from "./templates/sitemap-style.js";
import { sitemap } from "./templates/sitemap.js";
import { compile } from "./templating.js";
import { ensureDir } from "./utils.js";

export type ChangeFreq = "always" | "hourly" | "daily" | "weekly" | "monthly" | "yearly" | "never";

export interface UrlEntry {
	loc: URL;
	lastModified: Date;
	changeFreq: ChangeFreq;
	priority: number;
}

export const urlEntry = (content: Content): UrlEntry => {
	return {
		loc: content.url,
		lastModified: content.frontmatter.lastModified,
		changeFreq: content.contentType === "page" ? "yearly" : "monthly",
		priority: content.contentType === "page" ? 0.8 : 0.5,
	};
};

export const write_sitemap = async (entries: Array<UrlEntry>, site: Site) => {
	await ensureDir(PATHS.out);

	await fs.writeFile(`${PATHS.out}/sitemap.xml`, renderSitemap(sitemap(entries), { pretty: site.isDev }));
	await fs.writeFile(`${PATHS.out}/sitema-style.xsl`, sitemapStyle(site.assets.get("sitemap.css")));
};
