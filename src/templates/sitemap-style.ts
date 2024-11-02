import type { Asset } from "../asset.js";

export const sitemapStyle = (css: Asset | undefined) => `<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="3.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:sitemap="http://www.sitemaps.org/schemas/sitemap/0.9">
  <xsl:output method="html" version="1.0" encoding="UTF-8" indent="yes"/>
  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml" lang="en">
      <head>
        <title>Sitemap => Eons :: IO ()</title>
        <meta charset="utf-8"/>
        <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <link rel="stylesheet" href="${css?.dest.filename}" />
      </head>
      <body>
        <main>
          <h1>Sitemap</h1>
          <p>An overview over all the pages, posts, projects and whatnot on <a href="https://www.eons.io/">eons.io</a>.</p> 
          <span class="border"></span>
          <ul>
            <xsl:for-each select="/sitemap:urlset/sitemap:url">
              <li>
                <a class="link">
                  <xsl:attribute name="href">
                    <xsl:value-of select="sitemap:loc"/>
                  </xsl:attribute>
                  <xsl:value-of select="sitemap:loc"/>
                </a>
                <span class="updated">
                  Last updated:
                  <xsl:value-of select="sitemap:lastmod" />
                </span>
              </li>
            </xsl:for-each>
          </ul>
        </main>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
`;
