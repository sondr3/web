import fs from "node:fs";
import path from "node:path";
import log from "loglevel";
import polka from "polka";
import sirv, { type Options } from "sirv";
import WebSocket, { WebSocketServer } from "ws";
import { PATHS } from "./constants.js";
import type { FsEmitter } from "./watcher.js";

export class Server {
	private tx: FsEmitter;

	constructor(tx: FsEmitter) {
		this.tx = tx;
	}

	public start = (): void => {
		void httpServer();
		void websocketServer(this.tx);
	};
}

export function httpServer() {
	const opts: Options = {
		gzip: true,
		brotli: true,
		dev: true,
	};

	const onNoMatch: polka.Options["onNoMatch"] = (_req, res) => {
		const page = fs.readFileSync(path.join(PATHS.out, "404", "index.html"), "utf-8");
		res.statusCode = 404;
		res.setHeader("Content-Type", "text/html");
		res.end(page);
	};

	return polka({ onNoMatch })
		.use(sirv(PATHS.out, opts))
		.listen(3000, (err: unknown) => {
			if (err) throw err;
			log.info("server running on http://localhost:3000/");
		});
}

export function websocketServer(tx: FsEmitter) {
	log.info("Websocket server running on ws://localhost:3001/");
	const ws = new WebSocketServer({ port: 3001 });
	ws.on("connection", (socket) => wsHandler(socket, tx));
}

function wsHandler(socket: WebSocket, tx: FsEmitter) {
	socket.addEventListener("message", (event) => {
		if (event.data === "ping") {
			socket.send("pong");
		}
	});

	tx.on("update", () => {
		socket.send("reload");
	});
}
