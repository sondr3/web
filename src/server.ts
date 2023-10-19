import * as path from "std/path/mod.ts";
import * as fs from "std/fs/mod.ts";
import { PATHS } from "./constants.ts";
import { extname } from "std/path/mod.ts";

export function httpServer() {
  const server = Deno.listen({ port: 3000 });
  console.log(`File server running on http://localhost:3000/`);

  (async () => {
    for await (const conn of server) {
      void handleHttp(conn);
    }
  })();
}

export function websocketServer(tx: BroadcastChannel) {
  const webSocketServer = Deno.listen({ port: 3001 });
  console.log("Websocket server running on ws://localhost:3001/");
  (async () => {
    for await (const conn of webSocketServer) {
      void handleWebsocket(tx, conn);
    }
  })();
}

const respondWithFile = async (req: Deno.RequestEvent, path: string): Promise<boolean> => {
  if (!(await fileExists(path))) return false;
  const file = await Deno.open(path, { read: true });
  const readableStream = file.readable;
  const response = new Response(readableStream, {
    headers: new Headers({
      "Content-Type": matchExtension(path),
    }),
  });
  try {
    await req.respondWith(response);
  } catch { /* noop */ }

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

export async function handleWebsocket(tx: BroadcastChannel, conn: Deno.Conn) {
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

    tx.onmessage = (event) => {
      if (event.data.type === "reload" && socket.readyState === WebSocket.OPEN) {
        socket.send("reload");
      }
    };

    req.respondWith(response);
  }
}

const fileExists = async (filePath: string): Promise<boolean> => {
  try {
    return await fs.exists(filePath, { isFile: true });
  } catch (_e) {
    return false;
  }
};

const matchExtension = (extension: string | null): string => {
  const normalizedExtension = extension ? extname(extension).slice(1) : null;
  switch (normalizedExtension) {
    case "atom":
      return "application/atom+xml";
    case "css":
      return "text/css; charset=utf8";
    case "csv":
      return "text/csv; charset=utf8";
    case "gif":
      return "image/gif";
    case "gz":
      return "application/x-gzip";
    case "html":
      return "text/html; charset=utf8";
    case "ico":
      return "image/x-icon";
    case "jpeg":
    case "jpg":
      return "image/jpeg";
    case "js":
    case "cjs":
    case "mjs":
      return "application/javascript";
    case "json":
      return "application/json";
    case "mp4":
      return "video/mp4";
    case "mpeg":
    case "mpg":
      return "video/mpeg";
    case "png":
      return "image/png";
    case "svg":
      return "image/svg+xml";
    case "txt":
      return "text/plain; charset=utf8";
    case "woff":
      return "application/font-woff";
    case "woff2":
      return "application/font-woff2";
    case "sitemap":
    case "xml":
    case "xsl":
    case "xss":
      return "application/xml";
    default:
      return "application/octet-stream";
  }
};
