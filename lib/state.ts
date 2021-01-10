import path from "path"

import { Metadata } from "./content"

/**
 * Site state during building, used to look up styles, content etc.
 */
type State = {
  readonly styles: ReadonlyMap<string, string>
  readonly pages: ReadonlyMap<string, Metadata>
}

const initialState: State = {
  styles: new Map<string, string>(),
  pages: new Map<string, Metadata>(),
}

export const siteState = initialState

/**
 * Looks for a CSS file in the site state styles map. Needed because cache busting
 * can turn `style.css` into `style.abcdef123.css`.
 *
 * @param name - File name to look for
 * @returns The correct filename, or undefined if not found
 */
export const getStyle = (name: string): string | undefined => {
  const stylePath = siteState.styles.get(name)
  if (stylePath === undefined) return `/${name}`
  return `/${path.parse(stylePath).base}`
}
