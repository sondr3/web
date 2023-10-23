import { encodeHex } from "std/encoding/hex.ts";
import { crypto } from "std/crypto/mod.ts";
import { parse, toFileUrl } from "std/path/mod.ts";
import * as path from "std/path/mod.ts";

export class Path {
  private path: URL;

  constructor(path: string | URL) {
    if (typeof path === "string") {
      this.path = toFileUrl(path);
    } else {
      this.path = path;
    }
  }

  get inner() {
    return this.path;
  }

  public get stem() {
    return parse(this.path.pathname).name;
  }

  public get filename() {
    return parse(this.path.pathname).base;
  }

  public get ext() {
    return parse(this.path.pathname).ext;
  }

  public get dirname() {
    return parse(this.path.pathname).dir;
  }

  public get absolute(): string {
    return this.path.pathname;
  }

  public async digest(content: string): Promise<void> {
    const digest = await crypto.subtle.digest("MD5", new TextEncoder().encode(content));
    const hash = encodeHex(digest).slice(0, 8);
    this.path = toFileUrl(path.join(this.dirname, `${this.stem}.${hash}${this.ext}`));
  }

  public common(other: string | Path) {
    const otherAsString = typeof other === "string" ? other : other.absolute;
    const length = path.common([this.absolute, otherAsString]).length;
    return this.absolute.slice(length);
  }
}
