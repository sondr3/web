import { Colorize as C } from "./Colors";

export abstract class Logger {
  public static log(out: string): void {
    console.log(C.log(out));
  }

  public static debug(out: string): void {
    console.log(`${C.debug("[DEBUG]")}\t ${out}`);
  }

  public static info(out: string): void {
    console.log(`${C.info("[INFO]")}\t ${out}`);
  }

  public static error(out: string): void {
    console.error(`${C.error("[ERROR]")}\t ${out}`);
  }

  public static success(out: string): void {
    console.error(`${C.success("[INFO]")}\t ${out}`);
  }
}
