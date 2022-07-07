{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates.Sitemap
  ( sitemapTemplate,
  )
where

import Control.Lens
import Data.Text (Text)
import Lucid
import Lucid.Base (makeAttributes)
import Types (Sitemap)

urlSet_ :: Term arg result => arg -> result
urlSet_ = term "urlset"

url_ :: Term arg result => arg -> result
url_ = term "url"

loc_ :: Term arg result => arg -> result
loc_ = term "loc"

lastMod_ :: Term arg result => arg -> result
lastMod_ = term "lastmod"

changeFreq_ :: Term arg result => arg -> result
changeFreq_ = term "changefreq"

priority_ :: Term arg result => arg -> result
priority_ = term "priority"

xsi_ :: Text -> Attributes
xsi_ = makeAttributes "xmlns:xsi"

schemaLocation_ :: Text -> Attributes
schemaLocation_ = makeAttributes "xsi:schemaLocation"

sitemapTemplate :: Sitemap -> Html ()
sitemapTemplate meta =
  toHtmlRaw ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" :: Text)
    *> urlSet_
      [ xsi_ "http://www.w3.org/2001/XMLSchema-instance",
        schemaLocation_ "http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd",
        xmlns_ "http://www.sitemaps.org/schemas/sitemap/0.9"
      ]
      ( do
          url_
            ( do
                loc_ "https://www.eons.io/"
                lastMod_ (toHtml $ meta ^. #buildTime)
                changeFreq_ "monthly"
                priority_ "0.7"
            )
          url_
            ( do
                loc_ "https://www.eons.io/404/"
                lastMod_ (toHtml $ meta ^. #buildTime)
                changeFreq_ "monthly"
                priority_ "0.7"
            )
          mapM_
            ( \p ->
                url_
                  ( do
                      loc_ "https://www.eons.io/404/"
                      maybe mempty (\a -> lastMod_ (toHtml a)) (p ^. #modifiedAt)
                      changeFreq_ "monthly"
                      priority_ "0.7"
                  )
            )
            (meta ^. #pages)
      )
