import { Asciidoctor } from "asciidoctor/types";
import { promises as fs } from "node:fs";
import path, { extname } from "node:path";

import { Context } from "../context.js";
import { Duration } from "../utils/duration.js";
import { walkDir } from "../utils/fs.js";
import * as logger from "../utils/logger.js";
import { slugify } from "../utils/utils.js";
import { Asciidoc } from "./asciidoc.js";
import { config } from "./config.js";

export interface Frontmatter {
  title: string;
  description: string;
  draft: boolean;
  slug: string | null;
  created: Date | null;
  modified: Date | null;
  tags: Array<string> | null;
  category: string | null;
}

export interface Metadata {
  layout: string;
}

export class Content {
  readonly metadata: Metadata;
  readonly frontmatter: Frontmatter;
  readonly document: Asciidoctor.Document | string;

  constructor(metadata: Metadata, frontmatter: Frontmatter, doc: Asciidoctor.Document | string) {
    this.metadata = metadata;
    this.frontmatter = frontmatter;
    this.document = doc;
  }

  content = (): string => {
    return typeof this.document === "string" ? this.document : this.document.getContent();
  };

  url = (): string => {
    if (this.frontmatter.slug !== null) {
      return `/${slugify(this.frontmatter.slug)}/`.replace("//", "/");
    }

    const base = [this.frontmatter.category, this.frontmatter.title]
      .flatMap((it) => (it ? [slugify(it)] : []))
      .join("/");

    return `/${base}/`;
  };

  path = (): string => {
    const root = this.url();
    return `/${root}/index.html`;
  };

  title = (): string => `${this.frontmatter.title} => Eons :: IO ()`;

  type = (): string => {
    switch (this.metadata.layout) {
      case "post":
        return `article`;
      case "page":
      default:
        return `website`;
    }
  };

  isArticle = (): boolean => this.metadata.layout === "post";

  modifiedDate = (): string => {
    if (this.frontmatter.modified === null) return "";
    return this.frontmatter.modified.toISOString().split("T")[0];
  };

  createdDate = (): string => {
    if (this.frontmatter.created === null) return "";
    return this.frontmatter.created.toISOString().split("T")[0];
  };

  static fromLayout = (content: Content, layout: string): Content => {
    return new Content(content.metadata, content.frontmatter, layout);
  };
}

const getDate = (val: string | undefined): Date | null => {
  if (val === undefined) return null;
  return new Date(val);
};

export const decodeFrontmatter = (document: Record<string, string>): Frontmatter => {
  const attributes = new Map(Object.entries(document));

  if (!attributes.has("doctitle") || !attributes.has("description")) {
    throw new Error("Frontmatter is missing title or description");
  }

  return {
    title: attributes.get("doctitle") ?? "",
    description: attributes.get("description") ?? "",
    draft: Boolean(attributes.get("draft")) ?? false,
    slug: attributes.get("slug") ?? null,
    created: getDate(attributes.get("created")),
    modified: getDate(attributes.get("modified")),
    tags: attributes.get("tags")?.split(",") ?? null,
    category: attributes.get("category") ?? null,
  };
};

const convertToContent = (document: string | Buffer, asciidoc: Asciidoc): Content => {
  const doc = asciidoc.parse(document);
  const frontmatter = decodeFrontmatter(doc.getAttributes() as Record<string, string>);
  const layout = (doc.getAttribute("layout") as string) ?? "page";
  const meta = { layout: layout };

  return new Content(meta, frontmatter, doc);
};

export const buildPages = async ({ site, asciidoc }: Context): Promise<void> => {
  const pageDuration = new Duration();
  const pages = path.resolve(config().content.pages);
  const filter = (name: string) => extname(name) === ".adoc";

  for await (const page of walkDir(pages, filter)) {
    const document = await fs.readFile(page);
    const content = convertToContent(document, asciidoc);
    site.addPage(content);
  }

  pageDuration.end();
  logger.info("Finished rendering pages in", pageDuration.result());
};

export const buildPosts = async ({ site, asciidoc, config }: Context): Promise<void> => {
  const postDuration = new Duration();
  const pages = path.resolve(config.content.posts);
  const filter = (name: string) => extname(name) === ".adoc";

  for await (const page of walkDir(pages, filter)) {
    const document = await fs.readFile(page);
    const content = convertToContent(document, asciidoc);
    if (content.metadata.layout === null) content.metadata.layout = "post";
    if (config.production && content.frontmatter.draft) continue;
    site.addPost(content);
  }

  postDuration.end();
  logger.info("Finished rendering posts in", postDuration.result());
};
