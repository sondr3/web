import { assertEquals } from "std/assert/mod.ts";
import { Sitemap, UrlEntry } from "./sitemap.ts";
import { XmlDocument } from "./xml.ts";

const sitemap = (): XmlDocument => {
  const sitemap = new Sitemap([
    new UrlEntry({
      loc: new URL("https://www.eons.io/"),
      lastmod: "2023-07-01T00:00:00.000Z",
      changefreq: "yearly",
      priority: 0.8,
    }),
    new UrlEntry({
      loc: new URL("https://www.eons.io/about/"),
      lastmod: "2023-08-01T00:00:00.000Z",
      changefreq: "yearly",
      priority: 0.8,
    }),
  ]);

  return sitemap.toXml();
};

Deno.test("creates valid sitemap", () => {
  const expected = `<?xml version="1.0" encoding="UTF-8"?>
<?xml-stylesheet type="text/xsl" href="/sitemap-style.xsl"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9" xmlns:image="http://www.google.com/schemas/sitemap-image/1.1" xmlns:video="http://www.google.com/schemas/sitemap-video/1.1">
  <url>
    <loc>
      https://www.eons.io/
    </loc>
    <lastmod>
      2023-07-01T00:00:00.000Z
    </lastmod>
    <changefreq>
      yearly
    </changefreq>
    <priority>
      0.8
    </priority>
  </url>
  <url>
    <loc>
      https://www.eons.io/about/
    </loc>
    <lastmod>
      2023-08-01T00:00:00.000Z
    </lastmod>
    <changefreq>
      yearly
    </changefreq>
    <priority>
      0.8
    </priority>
  </url>
</urlset>`;

  assertEquals(expected, sitemap().render());
});
