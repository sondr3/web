import { Colorize as C } from "../utils/Colors"
import { LogLevel, LogManager } from "./LogManager"

/**
 * Interface for a log event, contains the message and which module the event
 * cam from alongside the log level.
 */
export interface LogEntry {
  level: LogLevel
  message: string
  module?: string
}

/**
 * Simple class to log events across various different log levels, e.g. only output
 * `trace` events if the application has set the minimum log level threshold to it or above.
 */
export class Logger {
  private logManager: LogManager
  private readonly module?: string
  private readonly levels: { [key: string]: number } = {
    none: 0,
    trace: 1,
    debug: 2,
    info: 3,
    warn: 4,
    error: 5,
  }

  constructor(logManager: LogManager, module?: string) {
    this.logManager = logManager
    this.module = module
  }

  /**
   * Converts a log level to an integer so we can compare allowed log level and
   * the entry's level.
   *
   * @param level - Log level for entry
   * @returns The converted log levels number
   */
  private levelToInt(level: LogLevel): number {
    return this.levels[level.toLowerCase()]
  }

  /**
   * Output a entry to the log manager if the log level allowes.
   *
   * @param logLevel - Which log level the message is
   * @param message - Log message
   */
  private emit(logLevel: LogLevel, message: string): void {
    if (this.levelToInt(logLevel) > this.levelToInt(this.logManager.options.minLevel)) return

    const entry: LogEntry = {
      level: logLevel,
      message: message,
      module: this.module,
    }

    this.logManager.emit("log", entry)
  }

  public log(message: string): void {
    this.emit("none", C.log(message))
  }

  public debug(message: string): void {
    this.emit("debug", message)
  }

  public error(message: string): void {
    this.emit("error", message)
  }

  public warn(message: string): void {
    this.emit("warn", message)
  }

  public trace(message: string): void {
    this.emit("trace", message)
  }
}
