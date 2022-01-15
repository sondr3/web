import { Asciidoc } from "./build/asciidoc.js";
import { Config, config } from "./build/config.js";
import { Site } from "./build/site.js";
import { Templating } from "./templates/templates.js";

export interface Context {
  site: Site;
  config: Config;
  asciidoc: Asciidoc;
  templating: Templating;
}

export const context = async (production: boolean) => {
  const cfg = config(production);
  const templating = new Templating();
  await templating.init(cfg);
  return {
    site: new Site(),
    config: cfg,
    asciidoc: new Asciidoc(),
    templating,
  };
};
