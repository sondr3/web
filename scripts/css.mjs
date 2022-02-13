import parcel from "@parcel/css";
import path from "node:path";
import { promises as fs } from "node:fs";

const styles = await fs.readFile(path.join(process.cwd(), "./site/scss/style.css"));

const { code } = parcel.transform({
  filename: "style.css",
  code: Buffer.from(styles),
  minify: true,
  sourceMap: false,
});

console.log(code.toString());
