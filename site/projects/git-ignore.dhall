let Project = ./Project.dhall

let gitIgnore
    : Project
    = { name = "git-ignore"
      , description =
          "Quickly and easily list and fetch .gitignore templates from gitignore.io"
      , technology = [ "rust", "git" ]
      , gitHub = "https://github.com/sondr3/git-ignore"
      }

in  gitIgnore
