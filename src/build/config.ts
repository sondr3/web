import path from "node:path";

export interface Config {
  out: string;
  production: boolean;
  url: string;
  templates: string;
  content: {
    root: string;
    posts: string;
    pages: string;
  };
  assets: {
    root: string;
    styles: string;
    fonts: string;
    js: string;
    images: string;
  };
}

const root = path.resolve(process.cwd());

export const config = (prod = false): Config => ({
  out: path.join(root, "build"),
  production: prod,
  url: prod ? "https://www.eons.io" : "http://localhost",
  templates: path.join(root, "templates"),
  content: {
    root: path.join(root, "content"),
    posts: path.join(root, "content", "posts"),
    pages: path.join(root, "content", "pages"),
  },
  assets: {
    root: path.join(root, "assets"),
    styles: path.join(root, "assets", "scss"),
    fonts: path.join(root, "assets", "fonts"),
    js: path.join(root, "assets", "js"),
    images: path.join(root, "assets", "images"),
  },
});
