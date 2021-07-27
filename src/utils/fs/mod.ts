export * from "./walkdir.ts";
export * from "./copyFiles.ts";

export class FSError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "FSError";
  }
}
