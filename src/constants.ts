import { resolve } from "std/path/mod.ts";
import { Path } from "./path.ts";

interface Paths {
  out: string;
  source: string;
  templates: string;
  public: string;
  styles: string;
  js: string;
  content: string;
  pages: string;
  posts: string;
}

export const PATHS: Paths = {
  out: resolve(Deno.cwd(), "dist"),
  source: resolve(Deno.cwd(), "site"),
  templates: resolve(Deno.cwd(), "site/templates"),
  public: resolve(Deno.cwd(), "site/public"),
  styles: resolve(Deno.cwd(), "site/styles"),
  js: resolve(Deno.cwd(), "site/js"),
  content: resolve(Deno.cwd(), "site/content"),
  pages: resolve(Deno.cwd(), "site/content/pages"),
  posts: resolve(Deno.cwd(), "site/content/posts"),
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
