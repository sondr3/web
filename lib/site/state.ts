import { Metadata } from "../content"

/**
 * Site state during building, used to look up styles, content etc.
 */
export type State = {
  readonly styles: Map<string, string>
  readonly pages: Map<string, Metadata>
}

export const initialState: State = {
  styles: new Map<string, string>(),
  pages: new Map<string, Metadata>(),
}
