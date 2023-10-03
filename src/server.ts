import * as path from "std/path/mod.ts";
import { PATHS } from "./constants.ts";
import { fileExists } from "./utils.ts";

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

    const notFoundResponse = new Response("404 Not Found", { status: 404 });
    await requestEvent.respondWith(notFoundResponse);
  }
}
