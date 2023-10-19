import { parse, toFileUrl } from "std/path/mod.ts";

export class Path {
  private path: URL;

  constructor(path: string | URL) {
    if (typeof path === "string") {
      this.path = toFileUrl(path);
    } else {
      this.path = path;
    }
  }

  public get inner() {
    return this.path;
  }

  public get stem() {
    return parse(this.path.pathname).name;
  }

  public get filename() {
    return parse(this.path.pathname).base;
  }

  public asAbsolute() {
    return this.path.pathname;
  }
}
