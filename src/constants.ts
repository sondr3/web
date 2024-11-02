import { resolve } from "node:path";
import { Path } from "./path.js";

interface Paths {
	out: string;
	source: string;
	public: string;
	styles: string;
	js: string;
	content: string;
	pages: string;
	posts: string;
}

export const PATHS: Paths = {
	out: resolve(process.cwd(), "dist"),
	source: resolve(process.cwd(), "site"),
	public: resolve(process.cwd(), "site/public"),
	styles: resolve(process.cwd(), "site/styles"),
	js: resolve(process.cwd(), "site/js"),
	content: resolve(process.cwd(), "site/content"),
	pages: resolve(process.cwd(), "site/content/pages"),
	posts: resolve(process.cwd(), "site/content/posts"),
};

export type AssetPath = {
	source: Path;
	dest: Path;
};

export type SCSS_FILE = "styles.css" | "sitemap.css";

export const SCSS_FILES: Record<SCSS_FILE, AssetPath> = {
	"styles.css": {
		source: new Path(resolve(PATHS.styles, "styles.scss")),
		dest: new Path(resolve(PATHS.out, "styles.css")),
	},
	"sitemap.css": {
		source: new Path(resolve(PATHS.styles, "sitemap.scss")),
		dest: new Path(resolve(PATHS.out, "sitemap.css")),
	},
};

export type JS_FILE = "livereload.js";

export const JS_FILES: Record<JS_FILE, AssetPath> = {
	"livereload.js": {
		source: new Path(resolve(PATHS.js, "livereload.js")),
		dest: new Path(resolve(PATHS.out, "livereload.js")),
	},
};
