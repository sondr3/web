import path from "path"

export interface About {
  intro: string
  name: string
  socials: {
    github: string
  }
}

export interface Config {
  out: string
  production: boolean
  meta: {
    title: string
    url: string
    author: About
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
    root: string
    style: string
    js: string
    images: string
  }
}

const root = path.resolve(process.cwd())

const defaultConfig: Config = {
  // Defaults to `./test` for... testing, shocking.
  out: "./test",
  production: false,
  meta: {
    author: {
      intro: `I make things`,
      name: "Sondre Nilsen",
      socials: {
        github: "https://github.com/sondr3/",
      },
    },
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
    root: path.join(root, "assets/"),
    js: path.join(root, "assets/js/"),
    style: path.join(root, "assets/scss/"),
    images: path.join(root, "assets/images/"),
  },
}

export const setConfig = (prod = false, out = "./test/", url = "http://localhost"): void => {
  config.out = out
  config.production = prod
  config.meta.url = url
}

export const getConfig = (): Config => config

const config: Config = defaultConfig
