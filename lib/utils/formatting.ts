import prettier from "prettier";

export const formatHtml = (source: string): string => {
  return prettier.format(source, { parser: "html" });
};
