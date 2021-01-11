import { EventEmitter } from "events"

import { Colorize as C } from "../utils/Colors"
import { LogEntry, Logger } from "./Logger"

/**
 * Logging levels, e.g. which events are logged to the loggers output.
 */
export type LogLevel = "none" | "trace" | "debug" | "warn" | "error"

/**
 * Configuration options to the log manager.
 */
type LogOptions = {
  readonly minLevel: LogLevel
}

/**
 * A wrapper around the builtin {@link https://nodejs.org/dist/latest/docs/api/events.html | EventEmitter}
 * in Node. Once configured this managers options are global and used by all loggers
 * using this manager.
 */
export class LogManager extends EventEmitter {
  private registered = false
  options: LogOptions = {
    minLevel: "none",
  }

  /**
   * Configure the manager, overriding the default settings.
   *
   * @param options - Options to set
   */
  public configure(options?: LogOptions): LogManager {
    this.options = Object.assign({}, this.options, options)
    return this
  }

  /**
   * Gets a new logger in a module by creating a new {@link Logger} with this
   * as the manager.
   *
   * @param module - Module that the logger is instantiated in
   */
  public getLogger(module?: string): Logger {
    return new Logger(this, module)
  }

  /**
   * Emitter used when getting a new log entry.
   *
   * @param listener - Entry that was pushed to the listener.
   */
  // eslint-disable-next-line no-unused-vars
  public onLogEntry(listener: (logEntry: LogEntry) => void): LogManager {
    this.on("log", listener)
    return this
  }

  /**
   * Registers a console logger so that all logging occurs to `STDOUT` and `STDERR`.
   */
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

  /**
   * Utility function to format the output used in the {@link registerConsoleLogger} function.
   *
   * @param entry - Log entry to format
   * @returns A pretty formatted entry
   */
  private static formatConsoleOutput(entry: LogEntry): string {
    const module = entry.module ? `[${entry.module}]` : ``
    const message = `[${new Date().toISOString()}] ${module}`
    switch (entry.level) {
      case "none":
        return `${C.log(`[INFO]`)}\t${message}\t${entry.message}`
      case "error":
        return `${C.error(`[ERROR]`)}\t${message}\t${entry.message}`
      case "warn":
        return `${C.error(`[WARN]`)}\t${message}\t${entry.message}`
      case "debug":
        return `${C.debug(`[DEBUG]`)}\t${message}\t${entry.message}`
      case "trace":
        return `${C.debug(`[TRACE]`)}\t${message}\t${entry.message}`
    }
  }
}
