import { Config, config } from "./config.js";
import { Content } from "./content.js";

export class Site {
  pages: Array<Content> = [];
  config: Config;
  style = "style.css";

  constructor(prod: boolean) {
    this.config = config(prod);
  }

  addPage(content: Content) {
    this.pages.push(content);
  }

  setStyle(name: string) {
    this.style = name;
  }
}
