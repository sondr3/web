import path from "path";

export interface Config {
  out: string;
  meta: {
    title: string;
    url: string;
    author: string;
  };
  content: {
    posts: string;
    pages: string;
    layouts: string;
    partials: string;
  };
  assets: {
    style: string;
    js: string;
  };
}

const partialConfig = <T extends Partial<Config>>(t: T) => t;

const root = path.resolve(process.cwd());

const sharedConfig = partialConfig({
  meta: {
    author: "Sondre Nilsen",
    url: "http://localhost",
    title: "EONS",
  },
  content: {
    posts: path.join(root, "posts"),
    pages: path.join(root, "content/pages/"),
    partials: path.join(root, "content/partials/"),
    layouts: path.join(root, "content/layouts/"),
  },
  assets: {
    js: path.join(root, "assets/js"),
    style: path.join(root, "assets/scss"),
  },
});

const devConfig = partialConfig({
  out: "./public",
  meta: {
    author: "Sondre Nilsen",
    url: "http://www.eons.io",
    title: "EONS",
  },
});

const testConfig = partialConfig({
  out: "./test",
  meta: {
    author: "Sondre Nilsen",
    url: "http://localhost",
    title: "EONS",
  },
});

const mergeConfig = (
  left: Pick<Config, "content" | "meta" | "assets">,
  right: Pick<Config, "out" | "meta">,
): Config => {
  return { ...left, ...right };
};

export const getConfig = (): Config => {
  if (process.env.NODE_ENV === "test") {
    return mergeConfig(sharedConfig, testConfig);
  } else if (process.env.NODE_ENV === "production") {
    return mergeConfig(sharedConfig, devConfig);
  }

  return mergeConfig(sharedConfig, devConfig);
};
