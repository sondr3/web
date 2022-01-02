type Command = "build" | "serve" | "clean"

/**
 * A simple CLI helper for parsing command line arguments and commands.
 */
export class CLI {
  /** Command to run, e.g. 'build', 'clean' */
  readonly command: Command
  /** Run with optimizations etc */
  readonly production: boolean

  /**
   * Constructs a CLI program, note that if the command used is invalid it
   * defaults to `build`.
   */
  constructor(argv: ReadonlyArray<string>) {
    this.command = CLI.stringToCommand(argv[2]) ?? "build"
    const options = argv.slice(2, argv.length)
    this.production = CLI.parseProd(options)
  }

  private static stringToCommand(input: string): Command | undefined {
    if (input === "build" || input === "serve" || input === "clean") {
      return input
    }

    return
  }

  private static parseProd(options: readonly string[]): boolean {
    return options.some((value) => value === "-p" || value === "--prod")
  }
}
