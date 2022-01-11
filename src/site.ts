import { Content } from "./content.js";

export class Site {
  pages: Map<string, Content> = new Map();

  addPage(content: Content) {
    this.pages.set(content.metadata.path, content);
  }
}
