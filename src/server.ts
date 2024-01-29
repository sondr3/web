import * as fs from "std/fs/mod.ts";
import * as log from "std/log/mod.ts";
import * as path from "std/path/mod.ts";
import { extname } from "std/path/mod.ts";
import { PATHS } from "./constants.ts";

const logger = log.getLogger();

export class Server {
  private tx: BroadcastChannel;

  constructor(tx: BroadcastChannel) {
    this.tx = tx;
  }

  public start = (): void => {
    void httpServer();
    void websocketServer(this.tx);
  };
}

export function httpServer() {
  logger.info(`File server running on http://localhost:3000/`);
  Deno.serve({ port: 3000 }, httpHandler);
}

export function websocketServer(tx: BroadcastChannel) {
  logger.info("Websocket server running on ws://localhost:3001/");
  Deno.serve({ port: 3001 }, (req) => wsHandler(req, tx));
}

const respondWithFile = async (_req: Request, path: string): Promise<Response | null> => {
  if (!(await fileExists(path))) return null;
  const file = await Deno.open(path, { read: true });
  const readableStream = file.readable;
  const response = new Response(readableStream, {
    headers: new Headers({
      "Content-Type": matchExtension(path),
    }),
  });

  return response;
};

async function httpHandler(req: Request): Promise<Response> {
  const url = new URL(req.url);
  const filepath = decodeURIComponent(url.pathname);

  const html = await respondWithFile(req, path.join(PATHS.out, filepath, "index.html"));
  if (html !== null) {
    return html;
  }

  const file = await respondWithFile(req, path.join(PATHS.out, filepath));
  if (file !== null) {
    return file;
  }

  const notFound = await Deno.open(path.join(PATHS.out, "404", "index.html"), { read: true });
  return new Response(notFound.readable, { status: 404 });
}

function wsHandler(req: Request, tx: BroadcastChannel): Response {
  if (req.headers.get("upgrade") != "websocket") {
    return new Response(null, { status: 501 });
  }
  const { socket, response } = Deno.upgradeWebSocket(req);
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

  return response;
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
