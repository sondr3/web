import EventEmitter from "node:events";
import { parse } from "node:path";
import { subscribe } from "@parcel/watcher";
import debounce from "debounce";
import { JavaScriptAsset } from "./asset.js";
import { JS_FILES, PATHS } from "./constants.js";
import { Content } from "./content.js";
import { logConfig } from "./logger.js";
import { Site } from "./site.js";

const logger = logConfig.getLogger("watcher");

export class FsEmitter extends EventEmitter {}

const debounceHandler = debounce(async (fn: () => Promise<void>, tx: FsEmitter) => {
	await fn();
	tx.emit("update");
}, 200);

export const startWatcher = (site: Site, tx: FsEmitter): void => {
	void watchTemplates(site, tx);
	void watchContent(site, tx);
	void watchSCSS(site, tx);
	void watchJS(site, tx);
	void watchPublic(site, tx);
};

const watchSCSS = async (site: Site, tx: FsEmitter): Promise<void> => {
	void subscribe(PATHS.styles, (_, events) => {
		console.log(`scss changed: ${events}`);
		for (const _event of events) {
			debounceHandler(async () => {
				logger.info("Rebuilding CSS");
				for await (const asset of site.collectCSS()) {
					await asset.write(site);
				}
			}, tx);
		}
	});
};

const watchJS = async (site: Site, tx: FsEmitter): Promise<void> => {
	void subscribe(PATHS.js, (_, events) => {
		for (const event of events) {
			debounceHandler(async () => {
				logger.info(`Rebuilding JS ${parse(event.path).base}`);
				const item = JS_FILES[event.path as keyof typeof JS_FILES];
				const asset = new JavaScriptAsset(item.source, item.dest);
				site.collectAsset(asset);
				await asset.write(site);
			}, tx);
		}
	});
};

const watchTemplates = async (site: Site, tx: FsEmitter): Promise<void> => {
	void subscribe(PATHS.templates, (_, events) => {
		for (const event of events) {
			debounceHandler(async () => {
				logger.info(`Template ${parse(event.path).base} changed, rebuilding`);
				await site.collectContents();
				await site.writeContent();
				await site.writeSitemap();
			}, tx);
		}
	});
};

const watchContent = async (site: Site, tx: FsEmitter): Promise<void> => {
	void subscribe(PATHS.content, (_err, events) => {
		for (const event of events) {
			debounceHandler(async () => {
				logger.info(`Rebuilding page ${parse(event.path).base}`);
				const content = await Content.fromPath(event.path, "page", site.url);
				await content.write(site);
				site.collectContent(content);
				await site.writeSitemap();
			}, tx);
		}
	});
};

const watchPublic = async (site: Site, tx: FsEmitter): Promise<void> => {
	void subscribe(PATHS.public, (_, events) => {
		for (const event of events) {
			debounceHandler(async () => {
				logger.info(`Rebuilding static file ${parse(event.path).base}`);
				await site.collectStaticFiles();
				await site.copyStaticAssets();
			}, tx);
		}
	});
};
