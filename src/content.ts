import { parse } from "std/toml/mod.ts";
import { createExtractor, Format, Parser } from "std/front_matter/mod.ts";
import { z } from "zod";
import * as path from "std/path/mod.ts";
import djot from "djot";
import { renderTemplate } from "./render.tsx";
import { Context } from "./context.ts";

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

export interface Content {
  source: string;
  outPath: string;
  url: string;
  contentType: "page" | "post";
  frontmatter: Frontmatter;
  content: string;
  rendered: string;
}

const extractToml = createExtractor({ [Format.TOML]: parse as Parser });

export const contentFromPath = async (filePath: string, kind: "page" | "post"): Promise<Content> => {
  const source = await Deno.readTextFile(filePath);
  const { attrs, body } = extractToml(source);
  const frontmatter = Frontmatter.safeParse(attrs);

  if (!frontmatter.success) {
    console.error(frontmatter.error);
    throw new Error(`Failed to parse frontmatter for ${filePath}`);
  }

  const stem = path.parse(filePath).name;
  let outPath: string;
  if (frontmatter.data.slug === undefined) {
    outPath = path.join(stem, "index.html");
  } else {
    outPath = path.join(frontmatter.data.slug, "index.html");
  }

  const outUrl = path.join(frontmatter.data.slug ?? stem, "/");

  return {
    source: filePath,
    url: outUrl,
    outPath: outPath,
    contentType: kind,
    frontmatter: frontmatter.data,
    content: body,
    rendered: djot.renderHTML(djot.parse(body)),
  };
};

export const renderContent = (content: Content, context: Context): string => {
  const rendered = renderTemplate(content, context);
  return rendered;
};
