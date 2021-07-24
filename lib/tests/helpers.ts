import fs from "fs"
import os from "os"
import path from "path"

import { Config, defaultConfig, initialState, setConfig, Site, State } from "../site"

/**
 * Create a temporary folder in your OS' temporary folder.
 *
 * @param prefix - Prefix of folder
 * @returns Directory path
 */
export const createTestDirectory = (prefix = "test"): string => {
  return fs.mkdtempSync(path.join(os.tmpdir(), prefix))
}

/**
 * A simple wrapper around the default config that sets `out` to be a
 * random temporary directory.
 */
export const testConfig = setConfig(defaultConfig, {
  out: createTestDirectory(),
})

/**
 * A simple wrapper around {@link Site} that overrides the default `out` to be
 * a temporary directory.
 *
 * @param config - Test configuration
 * @param state - Test state
 * @returns A test site configuration
 */
export const TestSite = (config: Config = testConfig, state: State = initialState): Site => {
  return new Site(config, state)
}
