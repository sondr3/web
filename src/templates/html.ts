/**
 * My handrolled JSX-like solution to templating. Its gloriously awful.
 *
 * @param input - Some HTML
 * @param values - Values that are coerced to strings
 * @returns The converted HTML
 */
export const html = (input: TemplateStringsArray, ...values: readonly unknown[]): string => {
  return input
    .map((string, index) => {
      const value = values[index];

      if (Array.isArray(value)) {
        return string.toString() + value.join("");
      } else if (typeof value === "string") {
        return string.toString() + value;
      } else if (typeof value === "number") {
        return string.toString() + value.toString();
      } else {
        return string;
      }
    })
    .join("");
};
