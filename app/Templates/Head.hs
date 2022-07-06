{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates.Head
  ( headTemplate,
  )
where

import Control.Lens
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import GHC.IO (unsafePerformIO)
import Lucid
import Lucid.Base (makeAttributes)
import Types (Content (..), SiteMeta)

as_ :: Text -> Attributes
as_ = makeAttributes "as"

property_ :: Text -> Attributes
property_ = makeAttributes "property"

themeScript :: Text
themeScript = unsafePerformIO $ TIO.readFile "app/Templates/theme.js"

headTemplate :: Content a => SiteMeta -> a -> Html ()
headTemplate meta content =
  head_
    ( do
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
        meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge,chrome=1"]

        link_ [rel_ "preload", as_ "style", href_ $ meta ^. #cssUrl]
        link_ [rel_ "preload", as_ "script", href_ $ meta ^. #themeUrl]
        link_ [rel_ "preload", as_ "font", type_ "font/woff2", crossorigin_ "anonymous", href_ $ "/fonts/Piazzolla.woff2"]
        link_ [rel_ "preload", as_ "font", type_ "font/woff2", crossorigin_ "anonymous", href_ $ "/fonts/Inconsolata.woff2"]

        link_ [rel_ "icon", href_ "/favicon.ico"]
        link_ [rel_ "icon", href_ "/icon.svg", sizes_ "any", type_ "image/svg+xml"]
        link_ [rel_ "alternate icon", type_ "image/png", href_ "/icon-192.png", sizes_ "192x192"]
        link_ [rel_ "alternate icon", type_ "image/png", href_ "/icon-512.png", sizes_ "512x512"]
        link_ [href_ "/apple-touch-icon.png", rel_ "apple-touch-icon", sizes_ "180x180"]

        link_ [rel_ "author", href_ "/humans.txt"]
        link_ [rel_ "stylesheet", href_ $ meta ^. #cssUrl]
        link_ [rel_ "canonical", href_ $ getUrl meta content]

        meta_ [name_ "author", content_ $ meta ^. #siteAuthor]
        meta_ [name_ "description", content_ $ getDescription content]

        title_ $ toHtml title

        meta_ [property_ "og:locale", content_ "en"]
        meta_ [property_ "og:type", content_ $ getType content]
        meta_ [property_ "og:site_name", content_ $ meta ^. #siteTitle]
        meta_ [property_ "og:title", content_ title]
        meta_ [property_ "og:description", content_ $ getDescription content]
        meta_ [property_ "og:url", content_ $ getUrl meta content]
        meta_ [property_ "og:image", content_ ""]

        maybe mempty (\a -> meta_ [property_ "article:published_time", content_ a]) (getCreatedAt content)
        maybe mempty (\a -> meta_ [property_ "article:modified_time", content_ a]) (getModifiedAt content)

        meta_ [name_ "twitter:card", content_ "summary_large_image"]
        meta_ [name_ "twitter:site", content_ "sondr3"]

        script_ themeScript
    )
  where
    title = getTitle content <> " => " <> meta ^. #siteTitle
