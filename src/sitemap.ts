import { PATHS } from "./constants.ts";
import { Content } from "./content.ts";
import * as fs from "std/fs/mod.ts";
import { WriteFromSite } from "./writeable.ts";
import { Site } from "./site.ts";
import { XmlDocument, XmlNode, XmlNodeBuilder, XmlProperty } from "./xml.ts";

export type ChangeFreq = "always" | "hourly" | "daily" | "weekly" | "monthly" | "yearly" | "never";

export interface UrlEntryProps {
  loc: URL;
  lastmod: string;
  changefreq: ChangeFreq;
  priority: number;
}

export class UrlEntry {
  private loc: URL;
  private lastmod: string;
  private changefreq: ChangeFreq;
  private priority: number;

  public constructor({ loc, lastmod, changefreq, priority }: UrlEntryProps) {
    this.loc = loc;
    this.lastmod = lastmod;
    this.changefreq = changefreq;
    this.priority = priority;
  }

  public static fromContent(content: Content): UrlEntry {
    return new UrlEntry({
      loc: content.url,
      lastmod: content.frontmatter.lastModified.toISOString(),
      changefreq: content.contentType === "page" ? "yearly" : "monthly",
      priority: content.contentType === "page" ? 0.8 : 0.5,
    });
  }

  private locToXml(): XmlNode {
    return new XmlNodeBuilder("loc").setValue(this.loc.toString()).build();
  }

  private lastmodToXml(): XmlNode | null {
    return this.lastmod !== null ? new XmlNodeBuilder("lastmod").setValue(this.lastmod).build() : null;
  }

  private changefreqToXml(): XmlNode | null {
    return this.changefreq !== null ? new XmlNodeBuilder("changefreq").setValue(this.changefreq).build() : null;
  }

  private priorityToXml(): XmlNode | null {
    return this.priority !== null ? new XmlNodeBuilder("priority").setValue(this.priority.toString()).build() : null;
  }

  public toXml(): XmlNode {
    return new XmlNodeBuilder("url")
      .addChild(this.locToXml())
      .addChild(this.lastmodToXml())
      .addChild(this.changefreqToXml())
      .addChild(this.priorityToXml())
      .build();
  }
}

export class Sitemap implements WriteFromSite {
  public entries: Array<UrlEntry>;

  public constructor(content: Iterable<Content>) {
    this.entries = Array.from(content)
      .filter((p) => !p.frontmatter.special)
      .map((page) => UrlEntry.fromContent(page));
  }

  public toXml(): XmlDocument {
    const document = new XmlDocument();
    document.addProperty(
      new XmlProperty({
        name: `xml-stylesheet`,
        values: new Map([[`href`, `/sitemap-style.xsl`], [`type`, `text/xsl`]]),
      }),
    );
    const urlset = new XmlNode({
      name: `urlset`,
      properties: new Map([
        ["xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9"],
        [`xmlns:image`, "http://www.google.com/schemas/sitemap-image/1.1"],
        [`xmlns:video`, "http://www.google.com/schemas/sitemap-video/1.1"],
      ]),
    });

    this.entries.forEach((entry) => urlset.addChild(entry.toXml()));
    document.addChild(urlset);

    return document;
  }

  public async write(_site: Site) {
    const sitemap = this.toXml().render();
    await fs.ensureDir(PATHS.out);
    await Deno.writeTextFile(`${PATHS.out}/sitemap.xml`, sitemap);
  }
}
