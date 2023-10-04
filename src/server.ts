import * as path from "std/path/mod.ts";
import { PATHS } from "./constants.ts";
import { fileExists } from "./utils.ts";

export function httpServer() {
  const server = Deno.listen({ port: 3000 });
  console.log(`File server running on http://localhost:3000/`);

  (async () => {
    for await (const conn of server) {
      void handleHttp(conn);
    }
  })();
}

export function websocketServer() {
  const webSocketServer = Deno.listen({ port: 3001 });
  console.log("Websocket server running on ws://localhost:3001/");
  (async () => {
    for await (const conn of webSocketServer) {
      void handleWebsocket(conn);
    }
  })();
}

const respondWithFile = async (req: Deno.RequestEvent, path: string): Promise<boolean> => {
  if (!(await fileExists(path))) return false;
  const file = await Deno.open(path, { read: true });
  const readableStream = file.readable;
  const response = new Response(readableStream);
  await req.respondWith(response);

  return true;
};

export async function handleHttp(conn: Deno.Conn) {
  const httpConn = Deno.serveHttp(conn);
  for await (const requestEvent of httpConn) {
    const url = new URL(requestEvent.request.url);
    const filepath = decodeURIComponent(url.pathname);

    if (await respondWithFile(requestEvent, path.join(PATHS.out, filepath, "index.html"))) {
      continue;
    }

    if (await respondWithFile(requestEvent, path.join(PATHS.out, filepath))) {
      continue;
    }

    const notFound = await Deno.open(path.join(PATHS.out, "404", "index.html"), { read: true });
    const notFoundResponse = new Response(notFound.readable, { status: 404 });
    await requestEvent.respondWith(notFoundResponse);
  }
}

export async function handleWebsocket(conn: Deno.Conn) {
  const wsConn = Deno.serveHttp(conn);
  for await (const req of wsConn) {
    if (req.request.headers.get("upgrade") != "websocket") {
      return new Response(null, { status: 501 });
    }
    const { socket, response } = Deno.upgradeWebSocket(req.request);
    socket.addEventListener("message", (event) => {
      if (event.data === "ping") {
        socket.send("pong");
      }
    });

    req.respondWith(response);
  }
}
