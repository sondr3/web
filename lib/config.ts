import path from "path";

export interface Config {
  out: string;
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

const root = path.resolve(process.cwd());

const content: Partial<Config> = {
  content: {
    posts: path.join(root, "posts"),
    pages: path.join(root, "content/pages/"),
    partials: path.join(root, "content/partials/"),
    layouts: path.join(root, "content/layouts/"),
  },
};

const devConfig: Partial<Config> = {
  out: "./public",
  assets: {
    js: "",
    style: "",
  },
};

const testConfig: Partial<Config> = {
  out: "./test",
  assets: {
    js: "",
    style: "",
  },
};

export const getConfig = (): Config => {
  if (process.env.NODE_ENV === "test") {
    return <Config>{ ...content, ...testConfig };
  } else if (process.env.NODE_ENV === "production") {
    return <Config>{ ...content, ...devConfig };
  }

  return <Config>{ ...content, ...devConfig };
};
