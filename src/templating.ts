import * as log from "std/log/mod.ts";
import { PATHS } from "./constants.ts";
import type { Frontmatter } from "./content.ts";

const logger = log.getLogger();

export function compile(template: string): (args: Record<string, unknown>) => string {
  return new Function(
    "args",
    `with (args) { return \`${template}\`; }`,
  ) as unknown as (args: Record<string, unknown>) => string;
}

export const createContext = (context: Record<string, unknown>): Record<string, unknown> => {
  return new Proxy(context, {
    has(_target, key) {
      if (key in globalThis) {
        return false;
      }
      return true;
    },
    get(target: Record<string, unknown>, key: string): unknown {
      return target[key];
    },
  });
};

export const render = async (template: Frontmatter["layout"], context: Record<string, unknown>): Promise<string> => {
  try {
    const baseTemplate = await Deno.readTextFile(`${PATHS.templates}/base.html`);
    const childTemplate = await Deno.readTextFile(`${PATHS.templates}/${template}.html`);

    const base = compile(baseTemplate);
    const child = compile(childTemplate);

    const ctx = createContext(context);

    const inner = child(ctx);
    const rendered = base({ ...ctx, content: inner });

    return rendered;
  } catch (e) {
    logger.error(e);
    throw e;
  }
};
