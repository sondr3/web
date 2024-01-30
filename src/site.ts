import * as fs from "node:fs/promises";
import * as path from "node:path";
import log from "loglevel";
import { type Asset, CssAsset, JavaScriptAsset, type StaticAsset } from "./asset.js";
import { JS_FILES, PATHS, SCSS_FILES } from "./constants.js";
import { Content } from "./content.js";
import { Path } from "./path.js";
import { urlEntry, write_sitemap } from "./sitemap.js";
import { ensureDir, fromAsyncIterable, walkDir } from "./utils.js";
import { write } from "./writeable.js";

export type Mode = "prod" | "dev";

export class Site {
	public assets: Map<string, Asset> = new Map();
	public content: Map<string, Content> = new Map();
	public staticFiles: Array<StaticAsset> = [];
	public mode: Mode;
	public url: URL;

	private constructor(mode: Mode) {
		this.mode = mode;
		this.url = new URL(mode === "prod" ? "https://www.eons.io" : "http://localhost:3000");
	}

	public static async create(mode: Mode): Promise<Site> {
		const site = new Site(mode);

		const start = performance.now();

		await site.collectAssets();
		await fromAsyncIterable(site.collectCSS());
		await site.collectStaticFiles();
		await site.collectContents();

		const end = performance.now();
		log.info(`Site creation took ${(end - start).toFixed(0)}ms`);

		return site;
	}

	public get isProd(): boolean {
		return this.mode === "prod";
	}

	public get isDev(): boolean {
		return this.mode === "dev";
	}

	public async write(): Promise<void> {
		await this.copyStaticAssets();
		await this.writeAssets();
		await this.writeContent();
		await this.writeSitemap();
	}

	public async *collectCSS(): AsyncGenerator<Asset> {
		for await (const entry of Object.values(SCSS_FILES)) {
			const asset = new CssAsset(entry.source, entry.dest);
			this.assets.set(entry.dest.filename, asset);
			yield asset;
		}
	}

	public async writeAssets(): Promise<void> {
		return await write(this.assets, this);
	}

	public async collectAssets(): Promise<void> {
		for await (const entry of Object.values(JS_FILES)) {
			const asset = new JavaScriptAsset(entry.source, entry.dest);
			this.collectAsset(asset);
			log.debug(`Collected asset ${asset.source.filename}`);
		}
	}

	public collectAsset(asset: Asset): void {
		this.assets.set(asset.source.filename, asset);
	}

	public async collectStaticFiles(): Promise<void> {
		for await (const entry of walkDir(PATHS.public)) {
			this.staticFiles.push({ path: new Path(entry), prefix: PATHS.public });
			log.debug(`Collected static file ${entry}`);
		}
	}

	public collectContent(content: Content): void {
		this.content.set(content.sourcePath.filename, content);
	}

	public async writeContent(): Promise<void> {
		return await write(this.content, this);
	}

	public async collectContents(): Promise<void> {
		for (const entry of await fs.readdir(PATHS.pages)) {
			const path = `${PATHS.pages}/${entry}`;
			const content = await Content.fromPath(path, "page", this.url);

			if (content.frontmatter.draft && this.isProd) continue;

			this.collectContent(content);
			log.debug(`Collected content ${content.sourcePath.filename}`);
		}
	}

	public copyStaticAssets = async (): Promise<void> => {
		await Promise.all(
			this.staticFiles.map(async (file) => {
				const relative = path.relative(file.prefix, file.path.absolute);
				const outPath = path.join(PATHS.out, relative);
				await ensureDir(path.dirname(outPath));
				await fs.copyFile(file.path.absolute, outPath);
			}),
		);
	};

	public async writeSitemap(): Promise<void> {
		const entries = Array.from(this.content.values())
			.filter((p) => !p.frontmatter.special)
			.map((page) => urlEntry(page));

		await write_sitemap(entries, this);
	}
}
