import { LogLevel } from "./logging/LogManager"

type Command = "build" | "develop" | "serve" | "clean"

const stringToCommand = (input: string): Command | null => {
  if (input === "build" || input === "develop" || input === "serve" || input === "clean") {
    return input
  }

  return null
}

const parseNoise = (options: string[]): LogLevel => {
  const noiseFlag = options.some((val) => val.startsWith("-n") || val.startsWith("--noise"))
  let noisiness = "none"
  if (noiseFlag) {
    const flag = options.reverse().find((val) => val.startsWith("-n") || val.startsWith("--noise")) ?? "-n=none"
    noisiness = flag.split("=")[1]
  }

  return noisiness as LogLevel
}

const parseProd = (options: string[]): boolean => {
  return options.some((val) => val === "-p" || val === "--prod")
}

/**
 * A simple CLI helper for building my website.
 */
export class CLI {
  /** Command to run, e.g. 'build', 'clean' */
  command: Command
  /** Log level, debug etc */
  noisiness: LogLevel
  /** Run with optimizations etc */
  production: boolean

  constructor(argv: Array<string>) {
    this.command = stringToCommand(argv[2]) ?? "develop"
    const options = argv.splice(2)
    this.noisiness = parseNoise(options)
    this.production = parseProd(options)
  }
}
