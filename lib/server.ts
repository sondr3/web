import fs from "fs"
import { getConfig } from "./config"
import { renderStyles } from "./assets"
import path from "path"
import { renderPages } from "./build"
import * as http from "http"
import { logging } from "./utils/logging"
import WebSocket from "ws"

const logger = logging.getLogger("server")
const config = getConfig()

const findMimetype = (mimetypes: Record<string, string>, extension: string): string | undefined => {
  for (const [key, val] of Object.entries(mimetypes)) {
    if (key === extension) {
      return val
    }
  }

  return
}

export class Server {
  private wss: WebSocket.Server
  private server: http.Server

  constructor() {
    this.wss = new WebSocket.Server({
      port: 3001,
    })
    this.server = this.buildServer()
  }

  run(): void {
    this.watch()
    this.server.listen(3000)
    logger.log(`Started server on http://localhost:3000/`)
  }

  broadcastShutdown(): void {
    this.wss.clients.forEach((c) => c.send("shutdown"))
  }

  private watch(): void {
    fs.watch(config.assets.style, async (type, name) => {
      if (name.endsWith("~")) return
      if (type === "change" || type === "rename") {
        logger.log(`Rendering ${name}`)
        await renderStyles(path.join(getConfig().assets.style, "style.scss"), false)
        this.broadcastReload()
      }
    })

    fs.watch(config.content.pages, async (type, name) => {
      if (name.endsWith("~")) return
      if (type === "change" || type === "rename") {
        logger.log(`Rendering ${name}`)
        await renderPages(false)
        this.broadcastReload()
      }
    })
  }

  private broadcastReload(): void {
    this.wss.clients.forEach((c) => c.send("reload"))
  }

  private buildServer(): http.Server {
    return http.createServer((request, response) => {
      let filePath = `.${request.url ?? ""}`

      if (filePath === "./") filePath = "./index.html"
      if (filePath.endsWith("/")) filePath += "index.html"

      const extension = String(path.extname(filePath)).toLowerCase()
      const mimetypes = {
        ".html": "text/html",
        ".js": "text/javascript",
        ".css": "text/css",
        ".json": "application/json",
        ".png": "image/png",
        ".jpg": "image/jpg",
        ".gif": "image/gif",
        ".svg": "image/svg+xml",
        ".wav": "audio/wav",
        ".mp4": "video/mp4",
        ".woff": "application/font-woff",
        ".ttf": "application/font-ttf",
        ".eot": "application/vnd.ms-fontobject",
        ".otf": "application/font-otf",
        ".wasm": "application/wasm",
      }

      console.log("test")

      const contentType = findMimetype(mimetypes, extension) ?? "application/octet-stream"

      fs.readFile(path.join(getConfig().out, filePath), (error, content) => {
        if (error) {
          if (error.code === "ENOENT") {
            fs.readFile("./404.html", (_err, cont) => {
              response.writeHead(404, { "Content-Type": mimetypes[".html"] })
              response.end(cont, "utf-8")
            })
          } else {
            response.writeHead(500)
            response.end("Something went terribly wrong...")
          }
        } else {
          response.writeHead(200, { "Content-Type": contentType })
          if (contentType === "text/html") {
            const resp = `${content.toString()}\n<script type="text/javascript" src="/js/livereload.js"></script>`
            response.end(resp, "utf-8")
          } else {
            response.end(content, "utf-8")
          }
        }
      })
    })
  }
}
