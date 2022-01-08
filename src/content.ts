import path, { extname } from "node:path";
import { Codec, date, GetType, optional, string } from "purify-ts/Codec.js";
import { EitherAsync } from "purify-ts/EitherAsync.js";

import { Asciidoc } from "./asciidoc.js";
import { config } from "./config.js";
import { readFile, walkDir } from "./fs.js";

export const Metadata = Codec.interface({
  doctitle: string,
  description: string,
  created: optional(date),
  modified: optional(date),
});

export type Metadata = GetType<typeof Metadata>;

const decodeMedata = (data: unknown): Metadata => {
  return Metadata.decode(data).caseOf({
    Left: (err) => {
      throw new Error(err);
    },
    Right: (data) => data,
  });
};

export const renderPages = (asciidoc: Asciidoc): EitherAsync<Error, void> =>
  EitherAsync(async ({ throwE }) => {
    const pages = path.resolve(config().content.pages);
    const filter = (name: string) => extname(name) === ".adoc";

    for await (const page of walkDir(pages, filter)) {
      try {
        await readFile(page)
          .map(async (document) => {
            const doc = asciidoc.parse(document);
            const meta = decodeMedata(doc.getAttributes());
            console.log(meta);
          })
          .mapLeft((e) => e);
      } catch (e) {
        return throwE(e as Error);
      }
    }

    return;
  });
