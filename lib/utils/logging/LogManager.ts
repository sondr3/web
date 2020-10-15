import { EventEmitter } from "events"
import { LogEntry, Logger } from "./Logger"
import { Colorize as C } from "../Colors"

export type LogLevel = "none" | "trace" | "debug" | "info" | "warn" | "error"

interface LogOptions {
  minLevel: LogLevel
}

export class LogManager extends EventEmitter {
  private registered = false
  private options: LogOptions = {
    minLevel: (process.env.NOISINESS ?? "none") as LogLevel,
  }

  public configure(options?: LogOptions): LogManager {
    this.options = Object.assign({}, this.options, options)
    return this
  }

  public getLogger(module?: string): Logger {
    return new Logger(this, this.options.minLevel, module)
  }

  public onLogEntry(listener: (logEntry: LogEntry) => void): LogManager {
    this.on("log", listener)
    return this
  }

  public registerConsoleLogger(): LogManager {
    if (this.registered) return this

    this.onLogEntry((entry) => {
      const output = LogManager.formatConsoleOutput(entry)
      switch (entry.level) {
        case "none":
          console.log(output)
          break
        case "error":
          console.error(output)
          break
        case "warn":
          console.warn(output)
          break
        case "info":
          console.info(output)
          break
        case "debug":
          console.debug(output)
          break
        case "trace":
          console.trace(output)
          break
      }
    })

    this.registered = true
    return this
  }

  private static formatConsoleOutput(entry: LogEntry): string {
    const module = entry.module ? `[${entry.module}]` : ``
    const message = `[${new Date().toISOString()}] ${module}`
    switch (entry.level) {
      case "none":
        return `${C.log(`[LOG]`)}\t${message}\t${entry.message}`
      case "error":
        return `${C.error(`[ERROR]`)}\t${message}\t${entry.message}`
      case "warn":
        return `${C.error(`[WARN]`)}\t${message}\t${entry.message}`
      case "info":
        return `${C.info(`[INFO]`)}\t${message}\t${entry.message}`
      case "debug":
        return `${C.debug(`[DEBUG]`)}\t${message}\t${entry.message}`
      case "trace":
        return `${C.debug(`[TRACE]`)}\t${message}\t${entry.message}`
    }
  }
}
