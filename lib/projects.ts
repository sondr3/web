import { apiEndpoint } from "./config"

export interface Project {
  owner: string
  id: string
  description: string
}

export interface ProjectData {
  name: string
  stars: number
  url: string
  license: string
  licenseId: string
  languages: Array<string>
  primaryLanguage: string
  createdAt: string
}

export const projects: Array<Project> = [
  {
    owner: "sondr3",
    id: "git-ignore",
    description: "Quickly and easily list and fetch .gitignore templates from gitignore.io",
  },
  {
    owner: "sondr3",
    id: "git-anger-management",
    description: "Ever wanted to know just how angry your commits are?",
  },
]

export const mergedProjects = async (): Promise<Array<Project & ProjectData>> => {
  return await Promise.all(
    projects.map(async (project) => {
      const res = await fetch(`${apiEndpoint}/repo/${project.owner}/${project.id}`)
      const data = (await res.json()) as ProjectData
      return { ...project, ...data }
    }),
  )
}
