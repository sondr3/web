import { resolve } from "std/path/mod.ts";

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
