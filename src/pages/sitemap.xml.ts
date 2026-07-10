import { getCollection, type CollectionEntry } from "astro:content";
import { Readable } from "node:stream";
import {
  SitemapStream,
  streamToPromise,
  type SitemapItem as _SitemapItem,
  EnumChangefreq,
} from "sitemap";

type SitemapItem = Omit<_SitemapItem, "img" | "links" | "video">;

const mapPage = (
  page: CollectionEntry<"pages">,
  baseUrl: string,
): SitemapItem => {
  return {
    url: `${baseUrl}/${page.data.slug ?? page.id}/`,
    lastmod: page.data.updatedAt?.toISOString(),
    changefreq: page.data?.changeFreq as EnumChangefreq,
    priority: page.data.priority,
  };
};

export async function GET() {
  const url = import.meta.env.DEV
    ? "http://localhost:4321"
    : import.meta.env.SITE;

  const rootPages: Array<SitemapItem> = [
    {
      url: `${url}/`,
      lastmod: new Date().toISOString(),
      changefreq: "yearly" as EnumChangefreq,
      priority: 0.8,
    },
  ];
  const pages = (await getCollection("pages", ({ data }) => !data.draft)).map(
    (page) => mapPage(page, url),
  );
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
