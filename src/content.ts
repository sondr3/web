import { parse } from "std/toml/mod.ts";
import { createExtractor, Format, Parser } from "std/front_matter/mod.ts";
import { z } from "zod";
import * as path from "std/path/mod.ts";
import djot from "djot";
import { renderTemplate } from "./render.tsx";
import { Site } from "./site.ts";
import * as log from "std/log/mod.ts";
import { Path } from "./path.ts";
import { PATHS } from "./constants.ts";
import { ensureDir } from "std/fs/mod.ts";
import { minifyHTML } from "./minify.ts";
import { WriteFromSite } from "./writeable.ts";

const logger = log.getLogger();
const extractToml = createExtractor({ [Format.TOML]: parse as Parser });

export const Frontmatter = z.object({
  title: z.string(),
  last_modified: z.date(),
  description: z.string(),
  subtitle: z.string().optional(),
  slug: z.string().optional(),
  layout: z.enum(["page", "index", "404"]).default("page"),
  special: z.boolean().default(false),
})
  .transform(({ last_modified, ...rest }) => ({ ...rest, lastModified: last_modified }));

export type Frontmatter = z.infer<typeof Frontmatter>;

export class Content implements WriteFromSite {
  public sourcePath: Path;
  public url: URL;
  public contentType: "page" | "post";
  public frontmatter: Frontmatter;

  private sourceContent: string;

  private constructor(
    source: Path,
    url: URL,
    contentType: "page" | "post",
    frontmatter: Frontmatter,
    body: string,
  ) {
    this.sourcePath = source;
    this.url = url;
    this.contentType = contentType;
    this.frontmatter = frontmatter;
    this.sourceContent = body;
  }

  public get outPath(): Path {
    if (this.frontmatter.slug === undefined) {
      return new Path(path.join(PATHS.out, this.sourcePath.stem, "index.html"));
    } else {
      return new Path(path.join(PATHS.out, this.frontmatter.slug, "index.html"));
    }
  }

  public get content() {
    return djot.renderHTML(djot.parse(this.sourceContent));
  }

  public render(site: Site) {
    return renderTemplate(this, site);
  }

  public async write(site: Site) {
    await ensureDir(path.dirname(this.outPath.absolute));
    let rendered = this.render(site);

    if (site.isProd) {
      rendered = await minifyHTML(rendered);
    }

    await Deno.writeTextFile(this.outPath.absolute, rendered);
  }

  public static async fromPath(filePath: string, kind: "page" | "post", baseURL: URL): Promise<Content> {
    const source = await Deno.readTextFile(filePath);
    const { attrs, body } = extractToml(source);
    const frontmatter = Frontmatter.safeParse(attrs);

    if (!frontmatter.success) {
      logger.error(frontmatter.error);
      throw new Error(`Failed to parse frontmatter for ${filePath}`);
    }

    const stem = path.parse(filePath).name + "/";
    const url = new URL(frontmatter.data.slug ?? stem, baseURL);

    return new Content(
      new Path(filePath),
      url,
      kind,
      frontmatter.data,
      body,
    );
  }
}
