export default interface Project {
  id?: string;
  name: string;
  description: string;
  stargazers?: number;
  technologies?: [string];
  github?: string;
}
