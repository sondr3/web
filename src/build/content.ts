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
  slug?: string;
  created?: Date;
  modified?: Date;
  tags?: Array<string>;
  category?: string;
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

  path = (): string => {
    if (this.frontmatter.slug) return `/${slugify(this.frontmatter.slug)}/index.html`;

    const base = [this.frontmatter.category, this.frontmatter.title]
      .flatMap((it) => (it ? [slugify(it)] : []))
      .join("/");
    return `/${slugify(base)}/index.html`;
  };

  title = (): string => `${this.frontmatter.title} => Eons :: IO ()`;
}

const getDate = (val: string | undefined): Date | undefined => {
  if (val === undefined) return undefined;
  return new Date(val);
};

export const decodeFrontmatter = (document: Asciidoctor.Document): Frontmatter => {
  const attributes = new Map(Object.entries(document.getAttributes() as Record<string, string>));

  if (!attributes.has("doctitle") || !attributes.has("description")) {
    throw new Error("Frontmatter is missing title or description");
  }

  return {
    title: attributes.get("doctitle") ?? "",
    description: attributes.get("description") ?? "",
    slug: attributes.get("slug"),
    created: getDate(attributes.get("created")),
    modified: getDate(attributes.get("modified")),
    tags: attributes.get("tags")?.split(","),
    category: attributes.get("category"),
  };
};

const convertToContent = (document: string, asciidoc: Asciidoc): Content => {
  const doc = asciidoc.parse(document);
  const frontmatter = decodeFrontmatter(doc);
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
