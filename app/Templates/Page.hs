{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates.Page
  ( pageTemplate,
  )
where

import Lucid
import Templates.Footer (footerScript, footerTemplate)
import Templates.Head (headTemplate)
import Templates.Header (headerTemplate)
import Types (Content (getContent, getTitle), SiteMeta)

mainBody :: Content a => a -> Html ()
mainBody content =
  main_
    [class_ "main"]
    ( do
        h1_ (toHtml $ getTitle content)
        (toHtml $ getContent content)
    )

pageTemplate :: Content a => SiteMeta -> a -> Html ()
pageTemplate meta content =
  doctype_
    *> ( html_
           [lang_ "en", data_ "theme" "light"]
           ( do
               headTemplate meta content
               body_
                 [class_ "root"]
                 ( do
                     headerTemplate
                     mainBody content
                     footerTemplate
                     footerScript meta
                 )
           )
       )
