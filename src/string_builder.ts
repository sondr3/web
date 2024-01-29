export class StringBuilder {
  private _inner: Array<string> = [];

  public append(str: string): void {
    this._inner.push(str);
  }

  public appendLine(str: string): void {
    this._inner.push(str);
    this._inner.push("\n");
  }

  public push(...strs: Array<string>): void {
    this._inner.push(...strs);
  }

  public toString(): string {
    return this._inner.join("");
  }
}
