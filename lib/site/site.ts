import { Config, defaultConfig } from "./config"
import { initialState, State } from "./state"

export interface Site {
  readonly config: Config
  state: State
}

export const initialSite: Site = {
  config: defaultConfig,
  state: initialState,
}

export const setSite = (config: Config = defaultConfig, state: State = initialState): Site => ({ config, state })
