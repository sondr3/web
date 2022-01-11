import { Asciidoctor } from "asciidoctor/types";
import path, { extname } from "node:path";
import { array, Codec, date, GetType, optional, string } from "purify-ts/Codec.js";
import { EitherAsync } from "purify-ts/EitherAsync.js";

import { readFile, walkDir } from "../utils/fs.js";
import { slugify } from "../utils/utils.js";
import { Asciidoc } from "./asciidoc.js";
import { config } from "./config.js";
import { Site } from "./site.js";

export type Layout = "page" | "post";

export const FrontmatterCodec = Codec.interface({
  doctitle: string,
  description: string,
  created: optional(date),
  modified: optional(date),
  tags: optional(array(string)),
  category: optional(string),
});

export interface Metadata {
  layout: Layout;
}

export type Frontmatter = GetType<typeof FrontmatterCodec>;

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
    const base = [this.frontmatter.category, this.frontmatter.doctitle]
      .flatMap((it) => (it ? [slugify(it)] : []))
      .join("/");
    return `/${base}/index.html`;
  };

  title = (): string => `${this.frontmatter.doctitle} => Eons :: IO ()`;
}

export const decodeFrontmatter = (data: unknown): Frontmatter => {
  return FrontmatterCodec.decode(data).caseOf({
    Left: (err) => {
      throw new Error(err);
    },
    Right: (data) => data,
  });
};

const convertToContent = (document: string, asciidoc: Asciidoc): Content => {
  const doc = asciidoc.parse(document);
  const frontmatter = decodeFrontmatter(doc.getAttributes());
  const layout = (doc.getAttribute("layout") as Layout) ?? "page";
  const meta = { layout: layout };

  return new Content(meta, frontmatter, doc);
};

export const buildPages = (site: Site, asciidoc: Asciidoc): EitherAsync<Error, void> =>
  EitherAsync(async ({ throwE }) => {
    const pages = path.resolve(config().content.pages);
    const filter = (name: string) => extname(name) === ".adoc";

    for await (const page of walkDir(pages, filter)) {
      try {
        await readFile(page)
          .map((document) => convertToContent(document, asciidoc))
          .map((content) => site.addPage(content))
          .mapLeft((e) => e);
      } catch (e) {
        return throwE(e as Error);
      }
    }

    return;
  });
