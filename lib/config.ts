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

const root = path.resolve(process.cwd())

const defaultConfig: Config = {
  out: "./test",
  production: false,
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
}

export const setConfig = (prod = false, out = "./test/", url = "http://localhost"): void => {
  config.out = out
  config.production = prod
  config.meta.url = url
}

export const getConfig = (): Config => config

const config: Config = defaultConfig
