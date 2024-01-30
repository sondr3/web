import * as fs from "node:fs/promises";
import { PATHS } from "./constants.js";
import { Content } from "./content.js";
import { Site } from "./site.js";
import { compile } from "./templating.js";
import { ensureDir } from "./utils.js";

export type ChangeFreq = "always" | "hourly" | "daily" | "weekly" | "monthly" | "yearly" | "never";

export interface UrlEntry {
	loc: URL;
	lastmod: string;
	changefreq: ChangeFreq;
	priority: number;
}

export const urlEntry = (content: Content): UrlEntry => {
	return {
		loc: content.url,
		lastmod: content.frontmatter.lastModified.toISOString(),
		changefreq: content.contentType === "page" ? "yearly" : "monthly",
		priority: content.contentType === "page" ? 0.8 : 0.5,
	};
};

export const write_sitemap = async (entries: Array<UrlEntry>, site: Site) => {
	await ensureDir(PATHS.out);

	const sitemap = await fs.readFile(`${PATHS.templates}/sitemap.xml`, "utf-8");
	const sitemap_template = compile(sitemap);
	const sitemap_rendered = sitemap_template({ entries });
	await fs.writeFile(`${PATHS.out}/sitemap.xml`, sitemap_rendered);

	const xsl = await fs.readFile(`${PATHS.templates}/sitemap-style.xsl`, "utf-8");
	const xsl_template = compile(xsl);
	const xsl_rendered = xsl_template({ css: site.assets.get("sitemap.css") });
	await fs.writeFile(`${PATHS.out}/sitemap-style.xsl`, xsl_rendered);
};
