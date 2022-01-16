import { promises as fs } from "node:fs";
import path from "node:path";

import { cacheBust } from "../utils/utils.js";
import { Content } from "./content.js";

export class Site {
  pages: Array<Content> = [];
  posts: Array<Content> = [];
  js: Map<string, string> = new Map();
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

  addJs = async (name: string, production: boolean) => {
    const dir = path.parse(name);
    if (!production) {
      const out = `${dir.name}.js`;
      this.js.set(out, out);
    } else {
      const content = await fs.readFile(name);
      const hash = cacheBust(content, production);
      this.js.set(`${dir.name}${dir.ext}`, `${dir.name}.${hash}.js`);
    }
  };
}
