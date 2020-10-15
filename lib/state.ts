import path from "path"

interface State {
  styles: Map<string, string>
  pages: Map<string, string>
}

const initialState: State = {
  styles: new Map<string, string>(),
  pages: new Map<string, string>(),
}

export const siteState = initialState

export const getStyle = (name: string): string | undefined => {
  const stylePath = siteState.styles.get(name)
  if (stylePath === undefined) return `/${name}`
  return `/${path.parse(stylePath).base}`
}
