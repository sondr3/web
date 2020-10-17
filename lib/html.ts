/**
 * My handrolled JSX-like solution to templating. Its gloriously awful.
 *
 * @param input - Some HTML
 * @param values - Values that are coerced to strings
 * @returns The converted HTML
 */
export const html = (input: TemplateStringsArray, ...values: unknown[]): string => {
  let output = ""
  input.forEach((string, i) => {
    const value = values[i]

    if (Array.isArray(value)) {
      output += string.toString() + value.join("")
    } else if (typeof value === "string") {
      output += string.toString() + value
    } else if (typeof value === "number") {
      output += string.toString() + value.toString()
    } else {
      output += string
    }
  })

  return output
}
