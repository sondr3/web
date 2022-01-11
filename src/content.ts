import { Asciidoctor } from "asciidoctor/types";
import path, { extname } from "node:path";
import { array, Codec, date, GetType, optional, string } from "purify-ts/Codec.js";
import { EitherAsync } from "purify-ts/EitherAsync.js";

import { Asciidoc } from "./asciidoc.js";
import { config } from "./config.js";
import { readFile, walkDir } from "./fs.js";
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
  path: string;
  layout: Layout;
}

export type Frontmatter = GetType<typeof FrontmatterCodec>;

export interface Content {
  metadata: Metadata;
  frontmatter: Frontmatter;
  document: Asciidoctor.Document;
}

const decodeFrontmatter = (data: unknown): Frontmatter => {
  return FrontmatterCodec.decode(data).caseOf({
    Left: (err) => {
      throw new Error(err);
    },
    Right: (data) => data,
  });
};

const convertToContent = (document: string, path: string, asciidoc: Asciidoc): Content => {
  const doc = asciidoc.parse(document);
  const meta = decodeFrontmatter(doc.getAttributes());
  const layout = (doc.getAttribute("layout") as Layout) ?? "page";

  return { document: doc, frontmatter: meta, metadata: { layout: layout, path: path } };
};

export const renderPages = (site: Site, asciidoc: Asciidoc): EitherAsync<Error, void> =>
  EitherAsync(async ({ throwE }) => {
    const pages = path.resolve(config().content.pages);
    const filter = (name: string) => extname(name) === ".adoc";

    for await (const page of walkDir(pages, filter)) {
      try {
        await readFile(page)
          .map((document) => convertToContent(document, page, asciidoc))
          .map((content) => site.addPage(content))
          .mapLeft((e) => e);
      } catch (e) {
        return throwE(e as Error);
      }
    }

    return;
  });
