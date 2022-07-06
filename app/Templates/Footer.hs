{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates.Footer
  ( footerTemplate,
    footerScript,
  )
where

import Control.Lens
import Lucid
import Types (SiteMeta ())

footerTemplate :: Html ()
footerTemplate =
  footer_
    [class_ "footer"]
    ( p_
        ( "Content (C) Sondre Aasemoen, licensed under "
            <> a_ [href_ "https://creativecommons.org/licenses/by-sa/4.0/"] "CC BY-SA 4.0"
        )
    )

footerScript :: SiteMeta -> Html ()
footerScript meta = script_ [type_ "text/javascript", src_ $ meta ^. #themeUrl] ("" :: String)
