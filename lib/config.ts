export interface Config {
  out: string;
  content: {
    posts: string;
    pages: string;
  };
  assets: {
    style: string;
    js: string;
  };
}

const devConfig: Config = {
  out: "./dist",
  content: {
    posts: "",
    pages: "",
  },
  assets: {
    js: "",
    style: "",
  },
};

const testConfig: Config = {
  out: "./dist/test",
  content: {
    posts: "",
    pages: "",
  },
  assets: {
    js: "",
    style: "",
  },
};

export const getConfig = (): Config => {
  if (process.env.NODE_ENV === "test") {
    return testConfig;
  } else if (process.env.NODE_ENV === "production") {
    return devConfig;
  }

  return devConfig;
};
