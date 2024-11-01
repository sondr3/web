import { type ChildrenOf, renderDocument } from "@sondr3/radiant";
import type { Context, Frontmatter } from "./content.js";
import { logConfig } from "./logger.js";
import { base } from "./templates/base.js";
import { page } from "./templates/content.js";
import { home } from "./templates/home.js";
import { notFound } from "./templates/not_found.js";

const logger = logConfig.getLogger("templating");

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
