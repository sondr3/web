import { strict as assert } from "node:assert"

import { CLI } from "./cli.js"

export function cliWithoutDefault() {
  const cli = new CLI(["/usr/bin/node", "index.js"])
  assert.equal(cli.command, "build")
  assert(!cli.production)
}

export function cliWithCommand() {
  const cli = new CLI(["/usr/bin/node", "index.js", "clean"])
  assert(!cli.production)
  assert.equal(cli.command, "clean")
}

export function cliWithProduction() {
  const cli = new CLI(["/usr/bin/node", "index.js", "build", "-p"])
  assert(cli.production)
}
