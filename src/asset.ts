import * as fs from "node:fs/promises";
import * as path from "node:path";
import * as sass from "sass";
import { minify } from "terser";
import { minifyCSS } from "./minify.js";
import { Path } from "./path.js";
import type { Mode, Site } from "./site.js";
import { ensureDir } from "./utils.js";
import type { WriteFromSite } from "./writeable.js";

export interface StaticAsset {
	path: Path;
	prefix: string;
}

export interface Asset extends WriteFromSite {
	source: Path;
	dest: Path;
	write(site: Site): Promise<void>;
}

abstract class AssetBase implements Asset {
	source: Path;
	dest: Path;

	public constructor(source: Path | string, dest: Path | string) {
		this.source = typeof source === "string" ? new Path(source) : source;
		this.dest = typeof dest === "string" ? new Path(dest) : dest;
	}

	abstract build(mode: Mode): Promise<string>;

	public async write(site: Site) {
		const content = await this.build(site.mode);
		await ensureDir(path.dirname(this.dest.absolute));
		await fs.writeFile(this.dest.absolute, content);
	}
}

export class JavaScriptAsset extends AssetBase {
	async build(mode: Mode): Promise<string> {
		const src = await fs.readFile(this.source.absolute, "utf-8");

		if (mode === "dev") {
			return src;
		}

		await this.dest.digest(src);
		const minified = await minify(src);

		if (!minified.code) {
			throw new Error("Could not minify JS");
		}

		return minified.code;
	}
}

export class CssAsset extends AssetBase {
	async build(mode: Mode): Promise<string> {
		const res = sass.compile(this.source.absolute);

		if (mode === "dev") {
			return res.css;
		}

		await this.dest.digest(res.css);
		return minifyCSS(res.css);
	}
}
