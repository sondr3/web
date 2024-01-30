import { App } from "@tinyhttp/app";
import log from "loglevel";
import sirv from "sirv";
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
	new App().use("/", sirv(PATHS.out)).listen(3000);
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
