import path from "path"

export interface Config {
  out: string
  production: boolean
  meta: {
    title: string
    url: string
    author: string
  }
  content: {
    posts: string
    pages: string
  }
  templates: {
    pages: string
    layouts: string
    partials: string
  }
  assets: {
    style: string
    js: string
    static: string
  }
}

const partialConfig = <T extends Partial<Config>>(t: T) => t

const root = path.resolve(process.cwd())

const sharedConfig = partialConfig({
  production: process.env.NODE_ENV === "production",
  meta: {
    author: "Sondre Nilsen",
    url: "http://localhost",
    title: "EONS",
  },
  content: {
    posts: path.join(root, "content/posts/"),
    pages: path.join(root, "content/pages/"),
  },
  templates: {
    pages: path.join(root, "templates/pages/"),
    partials: path.join(root, "templates/partials/"),
    layouts: path.join(root, "templates/layouts/"),
  },
  assets: {
    js: path.join(root, "assets/js/"),
    style: path.join(root, "assets/scss/"),
    static: path.join(root, "assets/static/"),
  },
})

const devConfig = partialConfig({
  out: path.resolve(root, "./public/"),
  meta: {
    author: "Sondre Nilsen",
    url: "http://www.eons.io",
    title: "EONS",
  },
})

const testConfig = partialConfig({
  out: path.resolve(root, "./test/"),
  meta: {
    author: "Sondre Nilsen",
    url: "http://localhost",
    title: "EONS",
  },
})

const mergeConfig = (
  left: Pick<Config, "production" | "content" | "meta" | "assets" | "templates">,
  right: Pick<Config, "out" | "meta">,
): Config => {
  return { ...left, ...right }
}

export const getConfig = (): Config => {
  switch (process.env.NODE_ENV) {
    case "test":
      return mergeConfig(sharedConfig, testConfig)
    case "production":
      return mergeConfig(sharedConfig, devConfig)
    default:
      return mergeConfig(sharedConfig, devConfig)
  }
}
