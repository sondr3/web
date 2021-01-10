const MS_PER_SEC = 1000000n

/**
 * A wrapper class around the builtin high resolution time in Node, used to calculate
 * how much time has passed between creation and invoking {@link Duration.end}.
 */
export class Duration {
  private readonly startTime = 0n
  private readonly finishTime = 0n

  constructor() {
    this.startTime = process.hrtime.bigint()
  }

  /**
   * Returns the prettified runtime of the duration.
   *
   * @returns - Formatted duration
   */
  public result(): string {
    const time = Math.trunc(Number((this.finishTime - this.startTime) / MS_PER_SEC))
    return prettyPrintDuration(time)
  }

  public end(): void {
    this.finishTime = process.hrtime.bigint()
  }
}

/**
 * Pretty prints a duration, turning `1635` into `1s 635ms`.
 *
 * @param duration - A number representing a duration
 * @returns The pretty formatted duration
 */
export function prettyPrintDuration(duration: number): string {
  const output: readonly string[] = []

  const seconds = Math.trunc(duration / 1000)
  if (seconds > 0) {
    output.push(`${seconds}s`)
  }

  const ms = duration - seconds * 1000
  output.push(`${ms}ms`)

  return output.join(" ")
}
