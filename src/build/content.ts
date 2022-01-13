import { Asciidoctor } from "asciidoctor/types";
import path, { extname } from "node:path";

import { readFile, walkDir } from "../utils/fs.js";
import { slugify } from "../utils/utils.js";
import { Asciidoc } from "./asciidoc.js";
import { config } from "./config.js";
import { Site } from "./site.js";

export type Layout = "page" | "post";

export interface Frontmatter {
  title: string;
  description: string;
  slug: string | null;
  created: Date | null;
  modified: Date | null;
  tags: Array<string> | null;
  category: string | null;
}

export interface Metadata {
  layout: Layout;
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

    return `/${slugify(base)}/`;
  };

  path = (): string => {
    const root = this.url();
    return `/${root}/index.html`;
  };

  title = (): string => `${this.frontmatter.title} => Eons :: IO ()`;

  type = (): string => {
    switch (this.metadata.layout) {
      case "post":
        return `<meta property='og:type' content='article' />`;
      case "page":
        return `<meta property='og:type' content='website' />`;
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
    slug: attributes.get("slug") ?? null,
    created: getDate(attributes.get("created")),
    modified: getDate(attributes.get("modified")),
    tags: attributes.get("tags")?.split(",") ?? null,
    category: attributes.get("category") ?? null,
  };
};

const convertToContent = (document: string, asciidoc: Asciidoc): Content => {
  const doc = asciidoc.parse(document);
  const frontmatter = decodeFrontmatter(doc.getAttributes() as Record<string, string>);
  const layout = (doc.getAttribute("layout") as Layout) ?? "page";
  const meta = { layout: layout };

  return new Content(meta, frontmatter, doc);
};

export const buildPages = async (site: Site, asciidoc: Asciidoc): Promise<Error | void> => {
  const pages = path.resolve(config().content.pages);
  const filter = (name: string) => extname(name) === ".adoc";

  for await (const page of walkDir(pages, filter)) {
    const document = await readFile(page);
    if (document instanceof Error) return document;
    const content = convertToContent(document, asciidoc);
    site.addPage(content);
  }
};
