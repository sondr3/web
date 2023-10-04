import { transform } from "lightningcss";
import htmlnano from "htmlnano";
import "terser";
import "svgo";

declare module "htmlnano" {
  let presets: Presets;

  export function process(
    html: string,
    opts: htmlnano.HtmlnanoOptions,
    preset: htmlnano.HtmlnanoPreset,
  ): Promise<{ html: string }>;
}

export const minifyCSS = (css: string): string => {
  const { code } = transform({
    filename: "styles.css",
    code: new TextEncoder().encode(css),
    minify: true,
  });

  return new TextDecoder().decode(code);
};

export const minifyHTML = async (html: string): Promise<string> => {
  const config: htmlnano.HtmlnanoOptions = {
    skipConfigLoading: true,
    minifyCss: false,
  };

  const res = await htmlnano.process(html, config, htmlnano.presets.safe);
  return res.html;
};
