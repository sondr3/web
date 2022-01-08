import path from "node:path";

interface Config {
  out: string;
  production: boolean;
  url: string;
  paths: {
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
  out: path.join(root, ".site"),
  production: prod,
  url: prod ? "https://www.eons.io" : "http://localhost",
  paths: {
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
