import EventEmitter from "node:events";
import { parse } from "node:path";
import { subscribe } from "@parcel/watcher";
import debounce from "debounce";
import log from "loglevel";
import { JavaScriptAsset } from "./asset.js";
import { JS_FILES, PATHS } from "./constants.js";
import { Content } from "./content.js";
import { Site } from "./site.js";

export class FsEmitter extends EventEmitter {}

const debounceHandler = debounce(async (fn: () => Promise<void>, tx: FsEmitter) => {
	await fn();
	tx.emit("update");
}, 200);

export class Watcher {
	private site: Site;
	private tx: FsEmitter;

	constructor(site: Site, tx: FsEmitter) {
		this.site = site;
		this.tx = tx;
	}

	public start = (): void => {
		void this.watchTemplates();
		void this.watchContent();
		void this.watchSCSS();
		void this.watchJS();
		void this.watchPublic();
	};

	private async watchSCSS() {
		void subscribe(PATHS.styles, (_, events) => {
			console.log(`scss changed: ${events}`);
			for (const _event of events) {
				debounceHandler(async () => {
					log.info("Rebuilding CSS");
					for await (const asset of this.site.collectCSS()) {
						await asset.write(this.site);
					}
				}, this.tx);
			}
		});
	}

	private async watchJS() {
		void subscribe(PATHS.js, (_, events) => {
			for (const event of events) {
				debounceHandler(async () => {
					log.info(`Rebuilding JS ${parse(event.path).base}`);
					const item = JS_FILES[event.path as keyof typeof JS_FILES];
					const asset = new JavaScriptAsset(item.source, item.dest);
					this.site.collectAsset(asset);
					await asset.write(this.site);
				}, this.tx);
			}
		});
	}

	private async watchTemplates() {
		void subscribe(PATHS.templates, (_, events) => {
			for (const event of events) {
				debounceHandler(async () => {
					log.info(`Template ${parse(event.path).base} chaged, rebuilding`);
					await this.site.collectContents();
					await this.site.writeContent();
					await this.site.writeSitemap();
				}, this.tx);
			}
		});
	}

	private async watchContent() {
		void subscribe(PATHS.content, (_err, events) => {
			for (const event of events) {
				debounceHandler(async () => {
					log.info(`Rebuilding page ${parse(event.path).base}`);
					const content = await Content.fromPath(event.path, "page", this.site.url);
					await content.write(this.site);
					this.site.collectContent(content);
					this.site.writeSitemap();
				}, this.tx);
			}
		});
	}

	private async watchPublic() {
		void subscribe(PATHS.public, (_, events) => {
			for (const event of events) {
				debounceHandler(async () => {
					log.info(`Rebuilding static file ${parse(event.path).base}`);
					await this.site.collectStaticFiles();
					await this.site.copyStaticAssets();
				}, this.tx);
			}
		});
	}
}
