let Page = ./Page.dhall

let about
    : Page
    = { title = "About me"
      , description = "Hello, I'm Sondre! I make things."
      , content = ""
      , slug = "about"
      , kind = "website"
      , createdAt = Some "2020-12-12"
      , modifiedAt = Some "2022-01-10"
      }

in  about
