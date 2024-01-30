import * as fs from "node:fs/promises";
import * as path from "node:path";
import log from "loglevel";
import { parse } from "smol-toml";
import { z } from "zod";
import { PATHS } from "./constants.js";
import { render_djot } from "./djot.js";
import { minifyHTML } from "./minify.js";
import { Path } from "./path.js";
import { Site } from "./site.js";
import { render } from "./templating.js";
import { ensureDir } from "./utils.js";
import type { WriteFromSite } from "./writeable.js";

const FRONTMATTER_DELIMITER = "+++";

export const Frontmatter = z
	.object({
		title: z.string(),
		last_modified: z.date(),
		description: z.string(),
		subtitle: z.string().optional(),
		slug: z.string().optional(),
		layout: z.enum(["page", "index", "404"]).default("page"),
		special: z.boolean().default(false),
		draft: z.boolean().default(false),
	})
	.transform(({ last_modified, ...rest }) => ({ ...rest, lastModified: last_modified }));

export type Frontmatter = z.infer<typeof Frontmatter>;

export class Content implements WriteFromSite {
	public sourcePath: Path;
	public url: URL;
	public contentType: "page" | "post";
	public frontmatter: Frontmatter;

	private sourceContent: string;

	private constructor(source: Path, url: URL, contentType: "page" | "post", frontmatter: Frontmatter, body: string) {
		this.sourcePath = source;
		this.url = url;
		this.contentType = contentType;
		this.frontmatter = frontmatter;
		this.sourceContent = body;
	}

	public get outPath(): Path {
		if (this.frontmatter.slug === undefined) {
			return new Path(path.join(PATHS.out, this.sourcePath.stem, "index.html"));
		}

		return new Path(path.join(PATHS.out, this.frontmatter.slug, "index.html"));
	}

	public get content() {
		return render_djot(this.sourceContent);
	}

	public context(site: Site) {
		return {
			title: `${this.frontmatter.title} => Eons :: IO ()`,
			canonicalUrl: new URL(this.url.toString(), site.url).toString(),
			css: site.assets.get("styles.css"),
			isDev: !site.isProd,
			content: this.content,
			frontmatter: this.frontmatter,
			pubDate: this.frontmatter.lastModified?.toISOString(),
		};
	}

	public async write(site: Site) {
		await ensureDir(path.dirname(this.outPath.absolute));
		let rendered = await render(this.frontmatter.layout, this.context(site));

		if (site.isProd) {
			rendered = await minifyHTML(rendered);
		}

		await fs.writeFile(this.outPath.absolute, rendered);
	}

	public static async fromPath(filePath: string, kind: "page" | "post", baseURL: URL): Promise<Content> {
		const source = await fs.readFile(filePath, "utf-8");

		const start = source.indexOf(FRONTMATTER_DELIMITER);
		const end = source.indexOf(FRONTMATTER_DELIMITER, start + FRONTMATTER_DELIMITER.length);
		const attrs = source.substring(start + FRONTMATTER_DELIMITER.length, end).trim();
		const body = source.substring(end + FRONTMATTER_DELIMITER.length).trim();

		const toml = parse(attrs);
		const frontmatter = Frontmatter.safeParse(toml);

		if (!frontmatter.success) {
			log.error(frontmatter.error);
			throw new Error(`Failed to parse frontmatter for ${filePath}`);
		}

		const stem = `${path.parse(filePath).name}/`;
		const url = new URL(frontmatter.data.slug ?? stem, baseURL);

		return new Content(new Path(filePath), url, kind, frontmatter.data, body);
	}
}
