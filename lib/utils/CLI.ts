import { LogLevel } from "./logging/LogManager"

/**
 * Commands that the CLI allows.
 */
type Command = "build" | "develop" | "serve" | "clean"

/**
 * A simple CLI helper for parsing command line arguments and commands.
 */
export class CLI {
  /** Command to run, e.g. 'build', 'clean' */
  command: Command
  /** Log level, debug etc */
  noisiness: LogLevel
  /** Run with optimizations etc */
  production: boolean

  /**
   * Constructs a CLI program, note that if the command used is invalid it
   * defaults to `develop`.
   *
   * @param argv - Command line arguments
   */
  constructor(argv: Array<string>) {
    this.command = CLI.stringToCommand(argv[2]) ?? "develop"
    const options = argv.splice(2)
    this.noisiness = CLI.parseNoise(options)
    this.production = CLI.parseProd(options)
  }

  /**
   * Checks if a string is a valid command, otherwise `null`.
   *
   * @param input - Command from command line
   * @returns `null` if `input` is not a valid command
   */
  private static stringToCommand(input: string): Command | null {
    if (input === "build" || input === "develop" || input === "serve" || input === "clean") {
      return input
    }

    return null
  }

  /**
   * Parse the noise flag from the command line, note that it has to be of the form
   * `-n=debug` or `--noise=debug`, using `-n debug` will result in it not being able to parse
   * it correctly. It will also use the last found noise flag, so you can override it by doing
   * `-n=debug -n=warn`, this will become `warn`.
   *
   * @param options - The arguments after the command from the CLI
   * @returns `none` if no valid log level is found.
   */
  private static parseNoise(options: string[]): LogLevel {
    const noiseFlag = options.some((val) => val.startsWith("-n") || val.startsWith("--noise"))
    let noisiness = "none"
    if (noiseFlag) {
      const flag = options.reverse().find((val) => val.startsWith("-n") || val.startsWith("--noise")) ?? "-n=none"
      noisiness = flag.split("=")[1]
    }

    return noisiness as LogLevel
  }

  /**
   * Checks if the command line arguments contain `-p` or `--prod`.
   *
   * @param options - The arguments after the command from the CLI
   */
  private static parseProd(options: string[]): boolean {
    return options.some((val) => val === "-p" || val === "--prod")
  }
}
