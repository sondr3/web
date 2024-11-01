import { Sitemap, s } from "@sondr3/radiant/sitemap";
import type { UrlEntry } from "../sitemap.js";

export const sitemap = (entries: Array<UrlEntry>): Sitemap =>
	s.document(
		s.doctype(),
		s.stylesheet("/sitemap-style.xsl", "text/xsl"),
		s.urlset(
			{
				xmlns: "http://www.sitemaps.org/schemas/sitemap/0.9",
				"xmlns:image": "http://www.google.com/schemas/sitemap-image/1.1",
				"xmlns:video": "http://www.google.com/schemas/sitemap-video/1.1",
			},
			...entries.map((entry) =>
				s.url(
					s.loc(entry.loc.toString()),
					s.lastmod(entry.lastModified),
					s.changefreq(entry.changeFreq),
					s.priority(entry.priority),
				),
			),
		),
	);
