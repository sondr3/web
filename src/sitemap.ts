import * as fs from "std/fs/mod.ts";
import { PATHS } from "./constants.ts";
import { Content } from "./content.ts";
import { Site } from "./site.ts";
import { compile } from "./templating.ts";

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
  await fs.ensureDir(PATHS.out);

  const sitemap = await Deno.readTextFile(`${PATHS.templates}/sitemap.xml`);
  const sitemap_template = compile(sitemap);
  const sitemap_rendered = sitemap_template({ entries });
  await Deno.writeTextFile(`${PATHS.out}/sitemap.xml`, sitemap_rendered);

  const xsl = await Deno.readTextFile(`${PATHS.templates}/sitemap-style.xsl`);
  const xsl_template = compile(xsl);
  const xsl_rendered = xsl_template({ css: site.assets.get("sitemap.css") });
  await Deno.writeTextFile(`${PATHS.out}/sitemap-style.xsl`, xsl_rendered);
};
