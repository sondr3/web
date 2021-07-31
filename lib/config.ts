export const isProd = process.env.VERCEL !== undefined

interface Config {
  endpoints: {
    contact: string
  }
  api: {
    prod: string
    dev: string
  }
}

export const config: Config = {
  endpoints: {
    contact: "contact",
  },
  api: {
    prod: "https://api.eons.io",
    dev: "http://localhost:8000",
  },
}

export const apiEndpoint = isProd ? config.api.prod : config.api.dev
