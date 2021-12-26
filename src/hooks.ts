import type { Handle } from "@sveltejs/kit";
import type { ServerResponse } from "@sveltejs/kit/types/hooks";
import { minify, Options } from "html-minifier-terser";

import { prerendering } from "$app/env";

const minificationOptions: Options = {
  collapseBooleanAttributes: true,
  collapseWhitespace: true,
  decodeEntities: true,
  html5: true,
  minifyCSS: true,
  minifyJS: true,
  removeAttributeQuotes: true,
  removeComments: true,
  removeEmptyAttributes: true,
  removeOptionalTags: true,
  removeRedundantAttributes: true,
  removeScriptTypeAttributes: true,
  removeStyleLinkTypeAttributes: true,
  sortAttributes: true,
  sortClassName: true,
  useShortDoctype: true,
};

export const handle: Handle = async ({ request, resolve }): Promise<ServerResponse> => {
  const response = await resolve(request);

  if (prerendering && response.headers["content-type"] === "text/html" && response.body) {
    response.body = await minify(response.body as string, minificationOptions);
  }

  return response;
};
