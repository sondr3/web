interface Color {
  normal: number;
  bright: number;
}

interface Format {
  start: number;
  end: number;
}

type FormatType = "reset" | "bold" | "underline" | "strike";

const format: Record<FormatType, Format> = {
  reset: {
    start: 0,
    end: 0,
  },
  bold: {
    start: 1,
    end: 22,
  },
  underline: {
    start: 4,
    end: 24,
  },
  strike: {
    start: 9,
    end: 29,
  },
};

type ColorType = "log" | "error" | "debug" | "info" | "success";

const colors: Record<ColorType, Color> = {
  log: {
    normal: 39,
    bright: 97,
  },
  error: {
    normal: 31,
    bright: 91,
  },
  debug: {
    normal: 33,
    bright: 93,
  },
  info: {
    normal: 36,
    bright: 96,
  },
  success: {
    normal: 32,
    bright: 92,
  },
};

export abstract class Colorize {
  public static log(input: string, bright = false): string {
    return this.format(input, colors.log, bright);
  }

  public static error(input: string, bright = false): string {
    return this.format(input, colors.error, bright);
  }

  public static debug(input: string, bright = false): string {
    return this.format(input, colors.debug, bright);
  }

  public static info(input: string, bright = false): string {
    return this.format(input, colors.info, bright);
  }

  private static getFormatting(start: number, end: number): { start: string; end: string } {
    return {
      start: `\x1b[${start}m`,
      end: `\x1b[${end}m`,
    };
  }

  private static format(input: string, color: Color, bright: boolean): string {
    const { start, end } = Colorize.getFormatting(bright ? color.bright : color.normal, format.reset.end);
    return `${start}${input}${end}`;
  }
}
