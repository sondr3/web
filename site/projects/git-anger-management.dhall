let Project = ./Project.dhall

let gitAngerManagement
    : Project
    = { name = "git-anger-management"
      , description =
          "Ever wanted to know just how angry your commits are? Look no further"
      , technology = [ "rust", "git" ]
      , gitHub = "https://github.com/sondr3/git-anger-management"
      }

in  gitAngerManagement
