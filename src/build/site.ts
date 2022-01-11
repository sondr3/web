import { Config, config } from "./config.js";
import { Content } from "./content.js";

export class Site {
  pages: Array<Content> = [];
  config: Config;
  style!: string;

  constructor(prod: boolean) {
    this.config = config(prod);
  }

  addPage(content: Content) {
    this.pages.push(content);
  }

  setStyle(name: string) {
    this.style = name;
  }

  url() {
    return this.config.production ? "https://www.eons.io" : "http://localhost:3000";
  }
}
