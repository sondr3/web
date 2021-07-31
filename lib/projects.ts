export interface Project {
  name: string
  owner: string
  description: string
  github: string
}

export const projects: Array<Project> = [
  {
    name: "git ignore",
    owner: "sondr3",
    description: "Quickly and easily list and fetch .gitignore templates from gitignore.io",
    github: "https://github.com/sondr3/git-ignore",
  },
  {
    name: "git anger management",
    owner: "sondr3",
    description: "Ever wanted to know just how angry your commits are?",
    github: "https://github.com/sondr3/git-anger-management",
  },
]
