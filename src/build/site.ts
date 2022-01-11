import { Config, config } from "./config.js";
import { Content } from "./content.js";

export class Site {
  pages: Array<Content> = [];
  config: Config;

  constructor(prod: boolean) {
    this.config = config(prod);
  }

  addPage(content: Content) {
    this.pages.push(content);
  }
}
