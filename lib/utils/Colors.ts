/**
 * Wrapper type for a 256 bit color used in the terminal, with a normal and a
 * bright value.
 */
interface Color {
  normal: number
  bright: number
}

/**
 * Wrapper type for formatting values used in the terminal, e.g. `bold` and so on.
 */
interface Format {
  start: number
  end: number
}

/**
 * Valid types of formatting for a string in the terminal.
 */
type FormatType = "reset" | "bold" | "underline" | "strike"

/**
 * The start and end value for various formatting values in the terminal.
 */
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
}

/**
 * Color types that is output to the terminal.
 */
type ColorType = "error" | "debug" | "info" | "success"

/**
 * The start and end value for the colors used to format text in the terminal.
 */
const colors: Record<ColorType, Color> = {
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
}

/**
 * Color a string, optionally using the bright variant of the colors.
 */
export abstract class Colorize {
  public static log(input: string, bright = false): string {
    return this.format(input, colors.info, bright)
  }

  public static error(input: string, bright = false): string {
    return this.format(input, colors.error, bright)
  }

  public static debug(input: string, bright = false): string {
    return this.format(input, colors.debug, bright)
  }

  /**
   * Get terminal escape sequences to format a string with.
   *
   * @param start - Start value
   * @param end - End value
   * @returns The wrapped sequence strings
   */
  private static getFormatting(start: number, end: number): { start: string; end: string } {
    return {
      start: `\x1b[${start}m`,
      end: `\x1b[${end}m`,
    }
  }

  /**
   * Format a string by wrapping it in a color.
   *
   * @param input - String to format
   * @param color - Color to wrap with
   * @param bright - Whether to use bright colors
   * @returns The formatted string
   */
  private static format(input: string, color: Color, bright: boolean): string {
    const { start, end } = Colorize.getFormatting(bright ? color.bright : color.normal, format.reset.end)
    return `${start}${input}${end}`
  }
}
