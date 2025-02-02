import type { AstroInstance, MarkdownInstance } from "astro";
import { Readable } from "node:stream";
import { SitemapStream, streamToPromise } from "sitemap";

const mapPageAlt = (
  { frontmatter, url }: AstroInstance & MarkdownInstance<Record<string, unknown>>,
  baseUrl: string,
) => {
  if (!url) {
    return null;
  }

  return {
    url: `${baseUrl}${url}`,
    lastmod: frontmatter?.lastMod ?? new Date(),
    changefreq: frontmatter?.changefreq ?? "yearly",
    priority: Number.parseInt((frontmatter?.priority ?? "0.6") as string),
  };
};

type Page = AstroInstance & MarkdownInstance<Record<string, unknown>>;

export async function GET() {
  const url = import.meta.env.DEV ? "http://localhost:4321" : import.meta.env.SITE;

  const rootPages = [
    {
      url: `${url}/`,
      lastmod: new Date(),
      changefreq: "yearly",
      priority: 0.8,
    },
  ];
  const pages = (Object.values(import.meta.glob("../pages/**/*.{md,mdx}", { eager: true })) as Array<Page>)
    .map((page) => mapPageAlt(page, url))
    .filter((page) => page !== null);
  const allPages = [...rootPages, ...pages];

  const sitemap = new SitemapStream({
    hostname: url,
    xslUrl: `${url}/sitemap.xsl`,
    xmlns: {
      news: false,
      xhtml: false,
      image: true,
      video: true,
    },
  });

  const stream = Readable.from(allPages).pipe(sitemap);
  const data = await streamToPromise(stream);
  const res = new TextDecoder().decode(data);

  return new Response(res, {
    headers: {
      "Content-Type": "application/xml",
    },
  });
}
