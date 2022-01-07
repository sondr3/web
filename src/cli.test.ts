import { test } from "@sondr3/minitest"
import { strict as assert } from "node:assert"

import { CLI } from "./cli.js"

test("cli without defaults", () => {
  const cli = new CLI(["/usr/bin/node", "index.js"])
  assert.equal(cli.command, "build")
  assert(!cli.production)
})

test("cli with command", () => {
  const cli = new CLI(["/usr/bin/node", "index.js", "clean"])
  assert(!cli.production)
  assert.equal(cli.command, "clean")
})

test("cli with production", () => {
  const cli = new CLI(["/usr/bin/node", "index.js", "build", "-p"])
  assert(cli.production)
})
