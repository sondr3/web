import watcher from "@parcel/watcher";
import http from "node:http";
import { parse } from "node:path";
import handler from "serve-handler";
import { WebSocketServer } from "ws";

import { Asciidoc } from "./build/asciidoc.js";
import { renderStyles } from "./build/assets.js";
import { renderPages } from "./build/build.js";
import { Site } from "./build/site.js";

export class Server {
  private readonly wss: WebSocketServer;
  private readonly server: http.Server;
  private readonly site: Site;

  constructor(site: Site) {
    this.site = site;
    this.wss = new WebSocketServer({ port: 3001 });
    this.server = http.createServer((req, res) => void handler(req, res, { public: "build" }));
  }

  start() {
    void this.watch();
    this.serve();
  }

  serve() {
    this.server.listen(3000);
    console.info(`Server started on http://localhost:3000`);
  }

  close() {
    this.server.close();
    this.wss.close();
  }

  broadcastShutdown(): void {
    this.wss.clients.forEach((c) => c.send("shutdown"));
  }

  private async watch(): Promise<void> {
    try {
      await watcher.subscribe(this.site.config.assets.root, async (err, events) => {
        if (err !== null) throw err;
        for (const { path } of events) {
          const dir = parse(path);
          if (dir.ext === ".scss") {
            console.log(`Rebuilding styles, ${dir.name}${dir.ext} changed`);
            await renderStyles(this.site);
          }
        }
      });

      await watcher.subscribe(this.site.config.content.root, async (err, events) => {
        if (err !== null) throw err;
        for (const { path } of events) {
          const dir = parse(path);
          if (dir.ext === ".adoc") {
            console.log(`Rebuilding page ${dir.name}${dir.ext} changed`);
            await renderPages(this.site, new Asciidoc());
          }
        }
      });
    } catch (error) {
      if (error instanceof Error && error.name === "AbortError") return;
      console.error(error);
    }
  }
}
