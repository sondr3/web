{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates.Header
  ( headerTemplate,
  )
where

import Data.Text (Text)
import Lucid
import Lucid.Base (makeAttributes)

ariaLabel_ :: Text -> Attributes
ariaLabel_ = makeAttributes "aria-label"

fill_ :: Text -> Attributes
fill_ = makeAttributes "fill"

viewBox_ :: Text -> Attributes
viewBox_ = makeAttributes "viewBox"

stroke_ :: Text -> Attributes
stroke_ = makeAttributes "stroke"

path_ :: Term arg result => arg -> result
path_ = term "path"

strokeLinecap_ :: Text -> Attributes
strokeLinecap_ = makeAttributes "stroke-linecap"

strokeLinejoin_ :: Text -> Attributes
strokeLinejoin_ = makeAttributes "stroke-linejoin"

strokeWidth_ :: Text -> Attributes
strokeWidth_ = makeAttributes "stroke-width"

d_ :: Text -> Attributes
d_ = makeAttributes "d"

moon :: Text
moon = "M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z"

sun :: Text
sun = "M20.354 15.354A9 9 0 018.646 3.646 9.003 9.003 0 0012 21a9.003 9.003 0 008.354-5.646z"

themeButton :: Text -> Text -> Html ()
themeButton variant svg =
  button_
    [class_ ("theme-btn " <> "-" <> variant), ariaLabel_ ("toggle " <> variant <> " theme")]
    ( svg_
        [ xmlns_ "http://www.w3.org/2000/svg",
          class_ "",
          fill_ "none",
          viewBox_ "0 0 24 24",
          stroke_ "currentColor",
          width_ "24"
        ]
        (path_ [strokeLinecap_ "round", strokeLinejoin_ "round", strokeWidth_ "2", d_ svg] mempty)
    )

headerTemplate :: Html ()
headerTemplate =
  header_
    [class_ "header"]
    ( do
        h1_ [class_ "title"] (a_ [href_ "/"] "EONS :: IO ()")

        nav_
          [class_ "nav"]
          ( ul_
              [class_ "nav__links"]
              ( do
                  -- li_ [class_ "nav_link"] (a_ [href_ "/projects/"] "projects")
                  -- li_ [class_ "nav_link"] (a_ [href_ "/posts/"] "posts")
                  li_ [class_ "nav_link"] (a_ [href_ "/about/"] "about")
              )
          )

        themeButton "light" moon
        themeButton "dark" sun
    )
