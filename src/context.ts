import { Asciidoc } from "./build/asciidoc.js";
import { Config, config } from "./build/config.js";
import { Site } from "./build/site.js";
import { Template } from "./templates/template.js";

export interface Context {
  site: Site;
  config: Config;
  asciidoc: Asciidoc;
  template: Template;
}

export const context = async (production: boolean): Promise<Context> => {
  const cfg = config(production);
  const template = new Template();
  await template.update(cfg);
  return {
    site: new Site(),
    config: cfg,
    asciidoc: new Asciidoc(),
    template,
  };
};
