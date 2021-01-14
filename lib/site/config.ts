import path from "path"

import type { DeepPartial } from "../utils"

export type About = {
  readonly intro: string
  readonly name: string
  readonly socials: {
    readonly github: string
  }
}

export type Config = {
  readonly out: string
  readonly production: boolean
  readonly url: string
  readonly meta: {
    readonly title: string
    readonly author: About
  }
  readonly content: {
    readonly posts: string
    readonly pages: string
  }
  readonly templates: {
    readonly pages: string
    readonly layouts: string
    readonly partials: string
  }
  readonly assets: {
    readonly root: string
    readonly style: string
    readonly js: string
    readonly images: string
  }
}

const root = path.resolve(process.cwd())

export const defaultConfig: Config = {
  // Defaults to `./test` for... testing, shocking.
  out: "./test",
  production: false,
  url: "http://localhost",
  meta: {
    author: {
      intro: `I make things`,
      name: "Sondre Nilsen",
      socials: {
        github: "https://github.com/sondr3/",
      },
    },
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

export const setConfig = (config: Config, options: DeepPartial<Config>): Config => {
  return <Config>{ ...config, ...options }
}
