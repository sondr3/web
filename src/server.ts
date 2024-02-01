import fs from "node:fs";
import path from "node:path";
import polka from "polka";
import sirv, { type Options } from "sirv";
import WebSocket, { WebSocketServer } from "ws";
import { PATHS } from "./constants.js";
import { logConfig } from "./logger.js";
import type { FsEmitter } from "./watcher.js";

const logger = logConfig.getLogger("server");

export const startServer = (tx: FsEmitter): void => {
	void httpServer();
	void websocketServer(tx);
};

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
			logger.info("server running on http://localhost:3000/");
		});
}

export function websocketServer(tx: FsEmitter) {
	logger.info("Websocket server running on ws://localhost:3001/");
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
