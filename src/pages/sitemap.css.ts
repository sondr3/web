import { resolve } from "node:path";
import * as sass from "sass";

export async function GET() {
  const cwd = import.meta.dirname;
  const styles = "../../styles/sitemap.scss";
  const path = resolve(cwd, styles);
  const res = sass.compile(path, {});

  return new Response(res.css, {
    headers: {
      "Content-Type": "text/css; charset=utf-8",
    },
  });
}
