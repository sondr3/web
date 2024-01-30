import * as fs from "node:fs/promises";
import log from "loglevel";
import { PATHS } from "./constants.js";
import type { Frontmatter } from "./content.js";

export function compile(template: string): (args: Record<string, unknown>) => string {
	return new Function("args", `with (args) { return \`${template}\`; }`) as unknown as (
		args: Record<string, unknown>,
	) => string;
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
		const baseTemplate = await fs.readFile(`${PATHS.templates}/base.html`, "utf-8");
		const childTemplate = await fs.readFile(`${PATHS.templates}/${template}.html`, "utf-8");

		const base = compile(baseTemplate);
		const child = compile(childTemplate);

		const ctx = createContext(context);

		const inner = child(ctx);
		const rendered = base({ ...ctx, content: inner });

		return rendered;
	} catch (e) {
		log.error(e);
		throw e;
	}
};
