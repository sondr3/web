import watcher from "@parcel/watcher";
import http from "node:http";
import { parse } from "node:path";
import handler from "serve-handler";
import { WebSocketServer } from "ws";

import { renderStyles } from "./build/assets.js";
import { renderPages, renderSpecialPages } from "./build/build.js";
import { Context } from "./context.js";

export class Server {
  private readonly wss: WebSocketServer;
  private readonly server: http.Server;
  private readonly context: Context;

  constructor(ctx: Context) {
    this.context = ctx;
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
    this.broadcastShutdown();
    this.server.close();
    this.wss.close();
  }

  private broadcastShutdown(): void {
    this.wss.clients.forEach((c) => c.send("shutdown"));
  }

  private broadcastReload(): void {
    this.wss.clients.forEach((c) => c.send("reload"));
  }

  private async watch(): Promise<void> {
    try {
      await watcher.subscribe(this.context.config.assets.root, async (err, events) => {
        if (err !== null) throw err;
        for (const { path } of events) {
          const dir = parse(path);
          if (dir.ext === ".scss") {
            console.log(`Rebuilding styles, ${dir.name}${dir.ext} changed`);
            await renderStyles(this.context);
          }
        }
        this.broadcastReload();
      });

      await watcher.subscribe(this.context.config.content.root, async (err, events) => {
        if (err !== null) throw err;
        for (const { path } of events) {
          const dir = parse(path);
          console.log(`Rebuilding page ${dir.name}${dir.ext}`);
          await renderPages(this.context);
        }
        this.broadcastReload();
      });

      await watcher.subscribe(this.context.config.templates, async (err, events) => {
        if (err !== null) throw err;
        await this.context.template.update(this.context.config);
        for (const { path } of events) {
          const dir = parse(path);
          console.log(`Rebuilding... template '${dir.name}' changed`);
          await renderSpecialPages(this.context);
          await renderPages(this.context);
        }
        this.broadcastReload();
      });
    } catch (error) {
      if (error instanceof Error && error.name === "AbortError") return;
      console.error(error);
    }
  }
}
