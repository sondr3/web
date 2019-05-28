export interface Project {
  id: number;
  name: string;
  description: string;
  technologies: string[];
  github: string;
}

export const projects: Project[] = [
  {
    id: 0,
    name: "git ignore",
    description: "A small and simple .gitignore generator",
    technologies: ["Rust"],
    github: "https://github.com/sondr3/git-ignore"
  },
  {
    id: 1,
    name: "git anger management",
    description: "Ever wanted to know just how angry your commits are?",
    technologies: ["Rust", "Git"],
    github: "https://github.com/sondr3/git-anger-management"
  }
];

export default projects;
