import * as fs from "node:fs/promises";
import { type ChildrenOf, type HTMLElement, type HTMLTag, renderDocument } from "@sondr3/radiant";
import { PATHS } from "./constants.js";
import type { Context, Frontmatter } from "./content.js";
import { logConfig } from "./logger.js";
import { base } from "./templates/base.js";
import { page } from "./templates/content.js";
import { home } from "./templates/home.js";
import { notFound } from "./templates/not_found.js";

const logger = logConfig.getLogger("templating");

export function compile(template: string): (args: Record<string, unknown>) => string {
	return new Function("args", `with (args) { return \`${template}\`; }`) as unknown as (
		args: Record<string, unknown>,
	) => string;
}

export const createContext = (context: Record<string, unknown>): Record<string, unknown> => {
	return new Proxy(context, {
		has(_target, key) {
			return !(key in globalThis);
		},
		get(target: Record<string, unknown>, key: string): unknown {
			return target[key];
		},
	});
};

export const render = async (template: Frontmatter["layout"], context: Context): Promise<string> => {
	try {
		let inner: ChildrenOf<"main">;
		switch (template) {
			case "page": {
				inner = page(context.content);
				break;
			}
			case "index": {
				inner = home();
				break;
			}
			case "404": {
				inner = notFound();
				break;
			}
			default: {
				throw new Error(`Unknown template: ${template}`);
			}
		}
		return renderDocument(base(context, inner));
	} catch (e) {
		logger.error(e);
		throw e;
	}
};
