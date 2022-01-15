import { Content } from "./content.js";

export class Site {
  pages: Array<Content> = [];
  posts: Array<Content> = [];
  style!: string;

  addPage(content: Content) {
    this.pages.push(content);
  }

  addPost(content: Content) {
    this.posts.push(content);
  }

  content(): Array<Content> {
    return this.pages.concat(this.posts);
  }

  setStyle(name: string) {
    this.style = name;
  }
}
