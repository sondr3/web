import { Colorize as C } from "../Colors"
import { LogLevel, LogManager } from "./LogManager"

export interface LogEntry {
  level: LogLevel
  message: string
  module?: string
}

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

  private levelToInt(minLevel: LogLevel): number {
    return this.levels[minLevel.toLowerCase()]
  }

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
