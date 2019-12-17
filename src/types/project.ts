export default interface Project {
  id?: string;
  name: string;
  description: string;
  technologies?: [string] | undefined;
  github?: string | undefined;
}
