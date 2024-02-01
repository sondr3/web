import { createHash } from "node:crypto";
import path, { parse } from "node:path";
import { pathToFileURL } from "node:url";

export class Path {
	private path: URL;

	constructor(path: string | URL) {
		this.path = typeof path === "string" ? pathToFileURL(path) : path;
	}

	get inner() {
		return this.path;
	}

	public get stem() {
		return parse(this.path.pathname).name;
	}

	public get filename() {
		return parse(this.path.pathname).base;
	}

	public get ext() {
		return parse(this.path.pathname).ext;
	}

	public get dirname() {
		return parse(this.path.pathname).dir;
	}

	public get absolute(): string {
		return this.path.pathname;
	}

	public async digest(content: string): Promise<void> {
		const hash = createHash("md5").update(content).digest("hex").slice(0, 8);
		this.path = pathToFileURL(path.join(this.dirname, `${this.stem}.${hash}${this.ext}`));
	}
}
