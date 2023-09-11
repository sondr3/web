import * as path from "std/path/mod.ts";
import * as fs from "std/fs/mod.ts";
import { PATHS } from "./constants.ts";

export async function handleHttp(conn: Deno.Conn) {
  const httpConn = Deno.serveHttp(conn);
  for await (const requestEvent of httpConn) {
    // Use the request pathname as filepath
    const url = new URL(requestEvent.request.url);
    const filepath = decodeURIComponent(url.pathname);

    let actualPath = "";
    try {
      if (await fs.exists(path.join(PATHS.out, filepath, "index.html"), { isFile: true })) {
        actualPath = path.join(PATHS.out, filepath, "index.html");
      }
    } catch {
      // noop
    }

    try {
      if (await fs.exists(path.join(PATHS.out, filepath), { isFile: true })) {
        actualPath = path.join(PATHS.out, filepath);
      }
    } catch {
      // noop
    }

    // Try opening the file
    let file;
    try {
      file = await Deno.open(actualPath, { read: true });
    } catch {
      // If the file cannot be opened, return a "404 Not Found" response
      const notFoundResponse = new Response("404 Not Found", { status: 404 });
      await requestEvent.respondWith(notFoundResponse);
      continue;
    }

    // Build a readable stream so the file doesn't have to be fully loaded into
    // memory while we send it
    const readableStream = file.readable;

    // Build and send the response
    const response = new Response(readableStream);
    await requestEvent.respondWith(response);
  }
}
