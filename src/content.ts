import { parse } from "std/toml/mod.ts";
import { createExtractor, Format, Parser } from "std/front_matter/mod.ts";
import { z } from "zod";
import { dirname } from "std/path/dirname.ts";
import djot from "djot";
import { renderTemplate } from "./render.tsx";

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
}

const extractToml = createExtractor({ [Format.TOML]: parse as Parser });

export const contentFromPath = async (path: string, kind: "page" | "post"): Promise<Content> => {
  const source = await Deno.readTextFile(path);
  const { attrs, body } = extractToml(source);
  const frontmatter = Frontmatter.safeParse(attrs);

  if (!frontmatter.success) {
    console.error(frontmatter.error);
    throw new Error(`Failed to parse frontmatter for ${path}`);
  }

  let outPath: string;
  if (!frontmatter.data.slug) {
    outPath = dirname(path);
  } else {
    outPath = frontmatter.data.slug;
  }

  const url = frontmatter.data.slug ?? path;

  return {
    source: path,
    url: url,
    outPath: outPath,
    contentType: kind,
    frontmatter: frontmatter.data,
    content: body ?? "",
  };
};

export const renderContent = (content: Content): string => {
  const html = djot.renderHTML(djot.parse(content.content));
  const rendered = renderTemplate(content.frontmatter, html);
  return rendered;
};
