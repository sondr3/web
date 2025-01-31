import { PATHS } from "./constants.js";
import { Path } from "./path.js";

import assert from "node:assert/strict";
import path from "node:path";
import test from "node:test";

test("path.digest", async () => {
	const it = new Path(path.join(PATHS.out, "./test.txt"));
	await it.digest("hello world");
	assert.equal(it.filename, "test.5eb63bbb.txt");
});
