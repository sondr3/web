{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates.Index
  ( indexTemplate,
  )
where

import Lucid
import Templates.Footer (footerScript, footerTemplate)
import Templates.Head (headTemplate)
import Templates.Header (headerTemplate)
import Types (Content, SiteMeta)

mainBody :: Html ()
mainBody =
  main_
    [class_ "main"]
    ( section_
        ( do
            h1_
              [class_ "hello"]
              ( do
                  span_ "Hello! I'm"
                  span_ [class_ "blue"] " Sondre"
              )
            p_ [class_ "me"] "I am a full time nerd with a passion for programming languages, mechanical keyboards, hoarding side-projects and occasionally creating useful software."
        )
    )

indexTemplate :: Content a => SiteMeta -> a -> Html ()
indexTemplate meta content =
  doctype_
    *> ( html_
           [lang_ "en", data_ "theme" "light"]
           ( do
               headTemplate meta content
               body_
                 [class_ "root"]
                 ( do
                     headerTemplate
                     mainBody
                     footerTemplate
                     footerScript meta
                 )
           )
       )
