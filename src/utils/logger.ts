import { color } from "../colors.js";

export const log = (...args: unknown[]) => {
  console.log(`${color("[LOG]", "green")}`, ...args);
};

export const error = (...args: unknown[]) => {
  console.error(`${color("[ERROR]", "red")}`, ...args);
};

export const warn = (...args: unknown[]) => {
  console.warn(`${color("[WARN]", "red")}`, ...args);
};

export const info = (...args: unknown[]) => {
  console.info(`${color("[INFO]", "blue")}`, ...args);
};
