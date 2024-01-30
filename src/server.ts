import { createServer } from "node:http";
import log from "loglevel";
import handler from "serve-handler";
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
	log.info("File server running on http://localhost:3000/");
	const server = createServer((req, res) => handler(req, res, { public: PATHS.out }));
	void server.listen(3000);
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
