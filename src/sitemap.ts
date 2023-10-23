import * as fs from "std/fs/mod.ts";
import { PATHS } from "./constants.ts";
import { Content } from "./content.ts";
import { Site } from "./site.ts";
import { WriteFromSite } from "./writeable.ts";
import {
  XmlContent,
  XmlDocument,
  XmlDocumentBuilder,
  XmlNode,
  XmlNodeBuilder,
  XmlProcessingInstruction,
} from "./xml.ts";

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
    return new XmlNodeBuilder("loc").withChild(new XmlContent(this.loc.toString())).build();
  }

  private lastmodToXml(): XmlNode | null {
    return this.lastmod !== null ? new XmlNodeBuilder("lastmod").withChild(new XmlContent(this.lastmod)).build() : null;
  }

  private changefreqToXml(): XmlNode | null {
    return this.changefreq !== null
      ? new XmlNodeBuilder("changefreq").withChild(new XmlContent(this.changefreq)).build()
      : null;
  }

  private priorityToXml(): XmlNode | null {
    return this.priority !== null
      ? new XmlNodeBuilder("priority").withChild(new XmlContent(this.priority.toString())).build()
      : null;
  }

  public toXml(): XmlNode {
    return new XmlNodeBuilder("url")
      .withChild(this.locToXml())
      .withChild(this.lastmodToXml())
      .withChild(this.changefreqToXml())
      .withChild(this.priorityToXml())
      .build();
  }
}

export class Sitemap implements WriteFromSite {
  public entries: Array<UrlEntry>;

  public constructor(entries: Array<UrlEntry> = []) {
    this.entries = entries;
  }

  public toXml(): XmlDocument {
    const document = new XmlDocumentBuilder()
      .withProcessingInstruction(
        new XmlProcessingInstruction({
          name: `xml-stylesheet`,
          value: `type="text/xsl" href="/sitemap-style.xsl"`,
        }),
      ).withChild(
        new XmlNodeBuilder("urlset")
          .withAttribute("xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")
          .withAttribute("xmlns:image", "http://www.google.com/schemas/sitemap-image/1.1")
          .withAttribute("xmlns:video", "http://www.google.com/schemas/sitemap-video/1.1")
          .withChildren(this.entries.map((entry) => entry.toXml()))
          .build(),
      );

    return document.build();
  }

  public async write(_site: Site) {
    const sitemap = this.toXml().render();
    await fs.ensureDir(PATHS.out);
    await Deno.writeTextFile(`${PATHS.out}/sitemap.xml`, sitemap);
  }
}
