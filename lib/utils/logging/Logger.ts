import { Colorize as C } from "../Colors";
import { Level, LogManager } from "./LogManager";
import { EventEmitter } from "events";

export interface LogEntry {
  level: Level;
  message: string;
  module?: string;
}

export class Logger {
  private logManager: EventEmitter;
  private readonly minLevel: number;
  private readonly module?: string;
  private readonly levels: { [key: string]: number } = {
    none: 0,
    trace: 1,
    debug: 2,
    info: 3,
    warn: 4,
    error: 5,
  };

  constructor(logManager: LogManager, minLevel: Level, module?: string) {
    this.logManager = logManager;
    this.module = module;
    this.minLevel = this.levelToInt(minLevel);
  }

  private levelToInt(minLevel: Level): number {
    return this.levels[minLevel.toLowerCase()];
  }

  private emit(logLevel: Level, message: string): void {
    if (this.levelToInt(logLevel) > this.minLevel) return;

    const entry: LogEntry = {
      level: logLevel,
      message: message,
      module: this.module,
    };

    this.logManager.emit("log", entry);
  }

  public log(message: string): void {
    this.emit("none", C.log(message));
  }

  public debug(message: string): void {
    this.emit("debug", message);
  }

  public info(message: string): void {
    this.emit("info", message);
  }

  public error(message: string): void {
    this.emit("error", message);
  }

  public warn(message: string): void {
    this.emit("warn", message);
  }

  public trace(message: string): void {
    this.emit("trace", message);
  }
}
