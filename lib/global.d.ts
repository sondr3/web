import { LogLevel } from "./utils/logging/LogManager";

export type NODE_ENV = "production" | "development" | "test";

declare namespace NodeJS {
  interface ProcessEnv {
    NODE_ENV?: NODE_ENV;
    NOISINESS?: LogLevel;
  }
}
