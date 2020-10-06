import { EventEmitter } from "events";
import { LogEntry, Logger } from "./Logger";
import { Colorize as C } from "../Colors";

export type Level = "none" | "trace" | "debug" | "info" | "warn" | "error";

interface LogOptions {
  minLevel: Level;
}

export class LogManager extends EventEmitter {
  private registered = false;
  private options: LogOptions = {
    minLevel: "none",
  };

  public configure(options?: LogOptions): LogManager {
    this.options = Object.assign({}, this.options, options);
    return this;
  }

  public getLogger(module?: string): Logger {
    return new Logger(this, this.options.minLevel, module);
  }

  public onLogEntry(listener: (logEntry: LogEntry) => void): LogManager {
    this.on("log", listener);
    return this;
  }

  public registerConsoleLogger(): LogManager {
    if (this.registered) return this;

    this.onLogEntry((entry) => {
      const output = LogManager.formatConsoleOutput(entry);
      switch (entry.level) {
        case "none":
          console.log(output);
          break;
        case "error":
          console.error(output);
          break;
        case "warn":
          console.warn(output);
          break;
        case "info":
          console.info(output);
          break;
        case "debug":
          console.debug(output);
          break;
        case "trace":
          console.trace(output);
          break;
      }
    });

    this.registered = true;
    return this;
  }

  private static formatConsoleOutput(entry: LogEntry): string {
    const module = entry.module ? `[${entry.module}]` : ``;
    const message = `[${new Date().toISOString()}] ${module}`;
    switch (entry.level) {
      case "none":
        return `${message} ${C.log(`[LOG]`)} ${entry.message}`;
      case "error":
        return `${message} ${C.error(`[ERROR]`)} ${entry.message}`;
      case "warn":
        return `${message} ${C.error(`[WARN]`)} ${entry.message}`;
      case "info":
        return `${message} ${C.info(`[INFO]`)} ${entry.message}`;
      case "debug":
        return `${message} ${C.debug(`[DEBUG]`)} ${entry.message}`;
      case "trace":
        return `${message} ${C.debug(`[TRACE]`)} ${entry.message}`;
    }
  }
}
