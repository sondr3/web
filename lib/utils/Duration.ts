const MS_PER_SEC = 1000000n;

export class Duration {
  private _startTime = 0n;
  private _finishTime = 0n;

  constructor() {
    this.startTime = process.hrtime.bigint();
  }

  public result(): string {
    const time = Math.trunc(Number((this.finishTime - this.startTime) / MS_PER_SEC));
    return prettyPrintDuration(time);
  }

  public end(): void {
    this.finishTime = process.hrtime.bigint();
  }

  private set startTime(value: bigint) {
    this._startTime = value;
  }

  private get startTime(): bigint {
    return this._startTime;
  }

  private get finishTime(): bigint {
    return this._finishTime;
  }

  private set finishTime(value: bigint) {
    this._finishTime = value;
  }
}

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
