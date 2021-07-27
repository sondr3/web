const MS_PER_SEC = 1000000;

/**
 * A wrapper class around the builtin high resolution time in Node, used to calculate
 * how much time has passed between creation and invoking {@link Duration.end}.
 */
export class Duration {
  private readonly startTime;
  private finishTime = 0;

  constructor() {
    this.startTime = performance.now();
  }

  /**
   * Returns the prettified runtime of the duration.
   *
   * @returns - Formatted duration
   */
  public result(): string {
    const time = Math.trunc(
      Number((this.finishTime - this.startTime) / MS_PER_SEC),
    );
    return prettyPrintDuration(time);
  }

  public end(): void {
    this.finishTime = performance.now();
  }
}

/**
 * Pretty prints a duration, turning `1635` into `1s 635ms`.
 *
 * @param duration - A number representing a duration
 * @returns The pretty formatted duration
 */
export function prettyPrintDuration(duration: number): string {
  const output: string[] = [];

  const seconds = Math.trunc(duration / 1000);
  if (seconds > 0) {
    output.push(`${seconds}s`);
  }

  const ms = duration - seconds * 1000;
  output.push(`${ms}ms`);

  return output.join(" ");
}
